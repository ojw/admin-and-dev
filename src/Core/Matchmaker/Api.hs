{-# LANGUAGE ScopedTypeVariables, DeriveDataTypeable, FlexibleContexts, OverloadedStrings #-}

module Core.Matchmaker.Api where

import Prelude hiding ( (.) )
import Control.Category ( (.) )
import Control.Monad.Trans
import Data.Data
import Data.Acid hiding ( query, update )
import Data.Acid.Advanced
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

import Happstack.Auth
import Core.Auth.Auth
import Util.HasAcidState
import Util.GetBody
import Core.Room.Acid
import Core.Matchmaker.Acid

processMatchmakerRequest :: (HasAcidState m RoomState, HasAcidState m AuthState, HasAcidState m ProfileState, HasAcidState m MatchmakerState, Happstack m, MonadIO m)
                   => UserId -> ByteString -> m Response
processMatchmakerRequest userId json =
    case decode json :: Maybe MatchmakerRequest of
        Nothing         -> ok $ toResponse $ ("Yeah that request body didn't have the right stuff." :: String)
        Just request    -> do t <- runMatchmakerAPI userId request -- will be uid roomId request
                              ok $ toResponse $ t
 
data MatchmakerRequest
    = RequestCreate Int
    | RequestDelete 
    | RequestJoin MatchmakerId
    | RequestLeave
    | RequestLook
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

runMatchmakerAPI :: (HasAcidState m MatchmakerState, HasAcidState m RoomState, MonadIO m, Happstack m) 
           => UserId -> MatchmakerRequest -> m Response -- Text
runMatchmakerAPI userId request =
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
                                               ok $ toResponse $ ("Should relocate..." :: Text) -- should relocate these players to lobby
            RequestJoin mId     -> do update (JoinMatchmaker userId mId)
                                      ok $ toResponse $ ("You're now there!" :: Text)
            RequestLeave        -> do mMatchmakerId <- query (GetMatchmakerByUser userId)
                                      case mMatchmakerId of
                                        Nothing     -> ok $ toResponse $ ("You aren't in that room." :: Text)
                                        Just mId    -> do
                                            toRelocate <- update (LeaveMatchmaker userId mId)
                                            ok $ toResponse $ ("We should relocate some people maybe..." :: Text) -- needs relocation
            RequestLook         -> do matchmakers <- query LookMatchmakers
                                      ok $ toResponse $ ("Here are some matchmakers..." :: Text) -- encode $ matchmakers
