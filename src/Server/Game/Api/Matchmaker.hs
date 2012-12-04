{-# LANGUAGE ScopedTypeVariables, DeriveDataTypeable, FlexibleContexts, OverloadedStrings #-}

module Server.Game.Api.Matchmaker where

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
import Happstack.Server.Response
import Web.Routes.Happstack

import Text.Boomerang.TH
import Web.Routes -- ( Site, runRouteT )
import Web.Routes.Boomerang

import Control.Monad ( mzero )
import Data.ByteString.Lazy as L

import Happstack.Auth

import Util.HasAcidState
import Server.Profile.Acid
import Server.Auth.Auth
import Server.Game.Acid.Types.Lobby
import Server.Game.Acid.Types.Room
import Server.Game.Acid.Types.Location
import Server.Game.Acid.Types.Matchmaker
import Server.Game.Acid.GameAcid
import Server.Game.Acid.Procedures
import Server.Game.Json.Matchmaker
 
data MatchmakerRequest
    -- = RequestCreate Int Int
    = RequestJoin MatchmakerId
    | RequestLeave
    | RequestLook
    deriving (Ord, Eq, Data, Typeable, Read, Show)

instance FromJSON MatchmakerRequest where
    parseJSON (Object o) =
        do
            (rqType :: Text) <- o .: "type"
            case rqType of
                {-
                "create"    -> do   cap <- o .: "capacity"
                                    required <- o .: "required"
                                    return $ RequestCreate (read cap :: Int) (read required :: Int)
                -}
                "join"      -> o .: "matchmaker" >>= \matchmakerId -> return $ RequestJoin (read matchmakerId :: MatchmakerId)
                "leave"     -> return RequestLeave
                "look"      -> return RequestLook
    parseJSON _ = mzero

processMatchmakerRequest 
    ::  (Happstack m, MonadIO m, Typeable p, Typeable s, Typeable o, SafeCopy p, SafeCopy s, SafeCopy o, HasAcidState m Server.Profile.Acid.ProfileState)
    =>  UserId -> AcidState (GameAcid p s o) -> ByteString -> m Response
processMatchmakerRequest userId gameAcid json =
    case decode json :: Maybe MatchmakerRequest of
        Nothing         -> badRequest $ toResponse $ ("That request body didn't have the right stuff." :: Text)
        Just request    -> runMatchmakerAPI userId gameAcid request

runMatchmakerAPI 
    ::  (MonadIO m, Happstack m, Typeable p, Typeable s, Typeable o, SafeCopy p, SafeCopy s, SafeCopy o, HasAcidState m Server.Profile.Acid.ProfileState)
    =>  UserId -> AcidState (GameAcid p s o) -> MatchmakerRequest -> m Response
runMatchmakerAPI userId gameAcid request =
        case request of
            --RequestCreate cap required  -> handleRequestCreate userId gameAcid cap required
            RequestJoin mId             -> handleRequestJoin userId gameAcid mId
            RequestLeave                -> handleRequestLeave userId gameAcid
            RequestLook                 -> handleRequestLook userId gameAcid

-- UPDATE with new room 
{-
handleRequestCreate 
    ::  (MonadIO m, Happstack m, Typeable p, Typeable s, Typeable o, SafeCopy p, SafeCopy s, SafeCopy o)
    =>  UserId -> AcidState (GameAcid p s o) -> Int -> Int -> m Response
handleRequestCreate userId gameAcid cap required = do
    loc <- query' gameAcid (GetLocation userId)
    case loc of
        Just (InLobby lobbyId)  -> do
            roomId <- update' gameAcid CreateRoom
            matchmakerId <- update' gameAcid (CreateMatchmaker userId cap required roomId lobbyId)
            update' gameAcid (SetLocation userId (Just (InMatchmaker matchmakerId)))
            ok $ toResponse $ ("Success" :: Text) -- stupid, should return matchmaker data to display
        _                       -> ok $ toResponse $ ("You must be in a lobby." :: Text)
-}

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
    ::  (MonadIO m, Happstack m, Typeable p, Typeable s, Typeable o, SafeCopy p, SafeCopy s, SafeCopy o, HasAcidState m Server.Profile.Acid.ProfileState)
    =>  UserId -> AcidState (GameAcid p s o) -> m Response
handleRequestLook userId gameAcid = do
    loc <- query' gameAcid (GetLocation userId)
    case loc of
        Just (InLobby lobbyId)  ->  do  matchmakers <- query' gameAcid (LookMatchmakers lobbyId)
                                        display <- mapM (displayMatchmaker gameAcid . _matchmakerId) matchmakers
                                        ok $ toResponse $ encode display -- ("FOO" :: Text) -- $ encode $ matchmakers
        _                       ->  ok $ toResponse ("Not in a lobby." :: Text)
