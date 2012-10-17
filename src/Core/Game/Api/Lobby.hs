{-# LANGUAGE ScopedTypeVariables, DeriveDataTypeable, FlexibleContexts, OverloadedStrings #-}

module Core.Game.Api.Lobby where

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
import Core.Game.Acid.Types.Lobby
import Core.Game.Acid.GameAcid
import Core.Game.Acid.Procedures
 
data LobbyRequest
    = RequestJoin LobbyId
    | RequestLook
    deriving (Ord, Eq, Data, Typeable, Read, Show)

instance FromJSON LobbyRequest where
    parseJSON (Object o) =
        do
            (rqType :: Text) <- o .: "type"
            case rqType of
                "join"      -> o .: "lobby" >>= \matchmakerId -> return $ RequestJoin (read matchmakerId :: LobbyId)
                "look"      -> return RequestLook
    parseJSON _ = mzero

processLobbyRequest 
    ::  (Happstack m, MonadIO m, Typeable p, Typeable s, Typeable o, SafeCopy p, SafeCopy s, SafeCopy o)
    =>  UserId -> AcidState (GameAcid p s o) -> ByteString -> m Response
processLobbyRequest userId gameAcid json =
    case decode json :: Maybe LobbyRequest of
        Nothing         -> ok $ toResponse $ ("That request body didn't have the right stuff." :: Text)
        Just request    -> runLobbyAPI userId gameAcid request

runLobbyAPI 
    ::  (MonadIO m, Happstack m, Typeable p, Typeable s, Typeable o, SafeCopy p, SafeCopy s, SafeCopy o)
    =>  UserId -> AcidState (GameAcid p s o) -> LobbyRequest -> m Response
runLobbyAPI userId gameAcid request =
        case request of
            RequestJoin lobbyId -> handleRequestJoin userId gameAcid lobbyId
            RequestLook         -> handleRequestLook userId gameAcid
 
handleRequestJoin
    ::  (MonadIO m, Happstack m, Typeable p, Typeable s, Typeable o, SafeCopy p, SafeCopy s, SafeCopy o)
    =>  UserId -> AcidState (GameAcid p s o) -> LobbyId -> m Response
handleRequestJoin userId gameAcid lobbyId = do
    update' gameAcid (SetLocation userId (Just (InLobby lobbyId)))
    ok $ toResponse $ ("Success" :: Text)

handleRequestLook
    ::  (MonadIO m, Happstack m, Typeable p, Typeable s, Typeable o, SafeCopy p, SafeCopy s, SafeCopy o)
    =>  UserId -> AcidState (GameAcid p s o) -> m Response
handleRequestLook userId gameAcid = do
    lobbies <- query' gameAcid LookLobbies
    ok $ toResponse ("FOO" :: Text) -- $ encode $ lobbies
