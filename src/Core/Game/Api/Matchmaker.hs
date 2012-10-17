{-# LANGUAGE ScopedTypeVariables, DeriveDataTypeable, FlexibleContexts, OverloadedStrings #-}

module Core.Game.Api.Matchmaker where

import Prelude hiding ( (.))
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

import Happstack.Auth
import Core.Auth.Auth
import Core.Game.Acid.Types.Lobby
import Core.Game.Acid.Types.Room
import Core.Game.Acid.Types.Location
import Core.Game.Acid.Types.Matchmaker
import Core.Game.Acid.GameAcid
import Core.Game.Acid.Procedures
 
data MatchmakerRequest
    = RequestCreate Int
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
                "join"      -> o .: "matchmaker" >>= \matchmakerId -> return $ RequestJoin (read matchmakerId :: MatchmakerId)
                "leave"     -> return RequestLeave
                "look"      -> return RequestLook
    parseJSON _ = mzero

processMatchmakerRequest 
    ::  (Happstack m, MonadIO m, Typeable p, Typeable s, Typeable o, SafeCopy p, SafeCopy s, SafeCopy o)
    =>  UserId -> AcidState (GameAcid p s o) -> ByteString -> m Response
processMatchmakerRequest userId gameAcid json =
    case decode json :: Maybe MatchmakerRequest of
        Nothing         -> ok $ toResponse $ ("That request body didn't have the right stuff." :: Text)
        Just request    -> runMatchmakerAPI userId gameAcid request

runMatchmakerAPI 
    ::  (MonadIO m, Happstack m, Typeable p, Typeable s, Typeable o, SafeCopy p, SafeCopy s, SafeCopy o)
    =>  UserId -> AcidState (GameAcid p s o) -> MatchmakerRequest -> m Response
runMatchmakerAPI userId gameAcid request =
        case request of
            RequestCreate cap   -> handleRequestCreate userId gameAcid cap
            RequestJoin mId     -> handleRequestJoin userId gameAcid mId
            RequestLeave        -> handleRequestLeave userId gameAcid
            RequestLook         -> handleRequestLook userId gameAcid
 
handleRequestCreate 
    ::  (MonadIO m, Happstack m, Typeable p, Typeable s, Typeable o, SafeCopy p, SafeCopy s, SafeCopy o)
    =>  UserId -> AcidState (GameAcid p s o) -> Int -> m Response
handleRequestCreate userId gameAcid cap = do
    loc <- query' gameAcid (GetLocation userId)
    case loc of
        Just (InLobby lobbyId)  -> do
            roomId <- update' gameAcid CreateRoom
            matchmakerId <- update' gameAcid (CreateMatchmaker userId cap roomId lobbyId)
            update' gameAcid (SetLocation userId (Just (InMatchmaker matchmakerId)))
            ok $ toResponse $ ("Success" :: Text) -- stupid, should return matchmaker data to display
        _                       -> ok $ toResponse $ ("You must be in a lobby." :: Text)

handleRequestJoin
    ::  (MonadIO m, Happstack m, Typeable p, Typeable s, Typeable o, SafeCopy p, SafeCopy s, SafeCopy o)
    =>  UserId -> AcidState (GameAcid p s o) -> MatchmakerId -> m Response
handleRequestJoin userId gameAcid mId = do
    hasCapacity <- query' gameAcid (MatchmakerHasCapacity mId)
    if hasCapacity 
    then do update' gameAcid (SetLocation userId (Just (InMatchmaker mId)))
            ok $ toResponse $ ("Success" :: Text) -- stupid, should return matchmaker data to display
    else ok $ toResponse $ ("It was full :/" :: Text)

handleRequestLeave
    ::  (MonadIO m, Happstack m, Typeable p, Typeable s, Typeable o, SafeCopy p, SafeCopy s, SafeCopy o)
    =>  UserId -> AcidState (GameAcid p s o) -> m Response
handleRequestLeave userId gameAcid = do
    mMatchmakerId <- query' gameAcid (GetLocation userId)
    case mMatchmakerId of
        Nothing                 -> ok $ toResponse $ ("You aren't in that room." :: Text)
        Just (InMatchmaker mId) -> do
            owner <- query' gameAcid (GetMatchmakerOwner mId)
            lobby <- query' gameAcid (GetMatchmakerLobbyId mId)
            if owner == Just userId
                then update' gameAcid (DeleteMatchmaker mId) >> return ()
                else do case lobby of
                            Nothing -> update' gameAcid (SetLocation userId Nothing)
                            Just l  -> update' gameAcid (SetLocation userId (Just (InLobby l)))
                        return ()
            ok $ toResponse $ ("Lobby data here." :: Text)

handleRequestLook
    ::  (MonadIO m, Happstack m, Typeable p, Typeable s, Typeable o, SafeCopy p, SafeCopy s, SafeCopy o)
    =>  UserId -> AcidState (GameAcid p s o) -> m Response
handleRequestLook userId gameAcid = do
    loc <- query' gameAcid (GetLocation userId)
    case loc of
        Just (InLobby lobbyId)  ->  do  matchmakers <- query' gameAcid (LookMatchmakers lobbyId)
                                        ok $ toResponse ("FOO" :: Text) -- $ encode $ matchmakers
        _                       ->  ok $ toResponse ("Not in a lobby." :: Text)
