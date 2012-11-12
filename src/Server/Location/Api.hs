{-# LANGUAGE ScopedTypeVariables, DeriveDataTypeable, FlexibleContexts, OverloadedStrings #-}

module Server.Location.Api where

import Prelude hiding ( (.) )
import Control.Category ( (.) )
import Control.Monad.Trans
import Control.Monad
import Data.Data
import Data.Acid hiding ( query, update )
import Data.Acid.Advanced
import Data.SafeCopy
import Data.Aeson
import Data.Text as Text
import Data.Text.Encoding   ( decodeUtf8 )

import Happstack.Server -- ( Happstack, Response )
import Web.Routes.Happstack

import Text.Boomerang.TH
import Web.Routes -- ( Site, runRouteT )
import Web.Routes.Boomerang

import Control.Monad ( mzero )
import Data.ByteString.Lazy as L

import Util.HasAcidState
import Util.GetBody
import Server.Auth.Acid       ( UserId )
import Server.Room.Acid       ( RoomId )
import Server.Lobby.Acid      ( LobbyId )
import Server.Location.Acid

processLocationRequest 
    ::  ( HasAcidState m MatchmakerState
        , Happstack m, MonadIO m
        , Typeable g, SafeCopy g
        , HasAcidState m (LobbyState g)
        , HasAcidState m (LocationState g)
        )
    =>  UserId -> ByteString -> m Response
processLocationRequest userId json =
    case decode json :: Maybe MatchmakerRequest of
        Nothing         -> ok $ toResponse $ ("Yeah that request body didn't have the right stuff." :: String)
        Just request    -> do t <- runMatchmakerAPI userId request lobby -- will be uid roomId request
                              ok $ toResponse $ t
 
data LocationRequest
    = RequestJoinLobby LobbyId
    | RequestJoinMatchmaker MatchmakerId
    | RequestJoinGame GameId
    deriving (Ord, Eq, Data, Typeable, Read, Show)

instance FromJSON MatchmakerRequest where
    parseJSON (Object o) =
        do
            (rqType :: Text) <- o .: "type"
            case rqType of
                "create"    -> o .: "capacity" >>= \cap -> return $ RequestCreate (read cap :: Int)
                "delete"    -> return RequestDelete
                "join"      -> o .: "matchmaker" >>= \matchmakerId -> return $ RequestJoin (read matchmakerId :: MatchmakerId)
                "leave"     -> return RequestLeave
                "look"      -> return RequestLook
    parseJSON _ = mzero

runMatchmakerAPI :: (HasAcidState m MatchmakerState, HasAcidState m RoomState, MonadIO m, Happstack m, Typeable g, SafeCopy g)
           => UserId -> MatchmakerRequest -> AcidState (Lobby g) -> m Response -- Text
runMatchmakerAPI userId request lobby =
    do
        matchmakerState :: AcidState MatchmakerState <- getAcidState
        case request of
            RequestCreate cap   -> do roomId <- update CreateRoom
                                      update' matchmakerState (CreateMatchmaker cap userId roomId)
                                      ok $ toResponse $ ("Success" :: Text)    -- so dumb
            RequestDelete       -> do ownedMatchmakerId <- query (GetMatchmakerByOwner userId)
                                      case ownedMatchmakerId of
                                        Nothing -> ok $ toResponse $ ("You don't own the room." :: Text)
                                        Just matchmakerId ->
                                            do toRelocate <- update' matchmakerState (DeleteMatchmaker matchmakerId)
                                               mapM (\u -> update' lobby (SetLocation u InLobby)) toRelocate
                                               ok $ toResponse $ ("Should relocate..." :: Text) -- should relocate these players to lobby
            RequestJoin mId     -> do update (JoinMatchmaker userId mId)
                                      ok $ toResponse $ ("You're now there!" :: Text)
            RequestLeave        -> do mMatchmakerId <- query (GetMatchmakerByUser userId)
                                      case mMatchmakerId of
                                        Nothing     -> ok $ toResponse $ ("You aren't in that room." :: Text)
                                        Just mId    -> do
                                            toRelocate <- update (LeaveMatchmaker userId mId)
                                            mapM (\u -> update' lobby (SetLocation u InLobby)) toRelocate
                                            ok $ toResponse $ ("We should relocate some people maybe..." :: Text) -- needs relocation
            RequestLook         -> do matchmakers <- query LookMatchmakers
                                      ok $ toResponse $ encode $ matchmakers
