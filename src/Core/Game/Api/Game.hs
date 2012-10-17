{-# LANGUAGE ScopedTypeVariables, DeriveDataTypeable, FlexibleContexts, OverloadedStrings #-}

module Core.Game.Api.Game where

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
import Core.Game.Acid.Types.Game
import Core.Game.Acid.GameAcid
import Core.Game.Acid.Procedures
 
data GameRequest
    = StartGame
    | QuitGame
    | RunCommand
    | GetDisplay
    deriving (Ord, Eq, Data, Typeable, Read, Show)

instance FromJSON GameRequest where
    parseJSON (Object o) =
        do
            (rqType :: Text) <- o .: "type"
            case rqType of
                "StartGame"     -> return StartGame
                "QuitGame"      -> return QuitGame
                "RunCommand"    -> return RunCommand
                "GetDisplay"    -> return GetDisplay
    parseJSON _ = mzero

processGameRequest 
    ::  (Happstack m, MonadIO m, Typeable p, Typeable s, Typeable o, SafeCopy p, SafeCopy s, SafeCopy o)
    =>  UserId -> AcidState (GameAcid p s o) -> ByteString -> m Response
processGameRequest userId gameAcid json =
    case decode json :: Maybe GameRequest of
        Nothing         -> ok $ toResponse $ ("That request body didn't have the right stuff." :: Text)
        Just request    -> runGameAPI userId gameAcid request

runGameAPI 
    ::  (MonadIO m, Happstack m, Typeable p, Typeable s, Typeable o, SafeCopy p, SafeCopy s, SafeCopy o)
    =>  UserId -> AcidState (GameAcid p s o) -> GameRequest -> m Response
runGameAPI userId gameAcid request =
        case request of
            StartGame   -> handleStartGame userId gameAcid
            QuitGame    -> handleQuitGame userId gameAcid
            RunCommand  -> handleRunCommand userId gameAcid
            GetDisplay  -> handleGetDisplay userId gameAcid
 
handleStartGame
    ::  (MonadIO m, Happstack m, Typeable p, Typeable s, Typeable o, SafeCopy p, SafeCopy s, SafeCopy o)
    =>  UserId -> AcidState (GameAcid p s o) -> m Response
handleStartGame userId gameAcid = do
    ok $ toResponse $ ("Placeholder." :: Text)

handleQuitGame
    ::  (MonadIO m, Happstack m, Typeable p, Typeable s, Typeable o, SafeCopy p, SafeCopy s, SafeCopy o)
    =>  UserId -> AcidState (GameAcid p s o) -> m Response
handleQuitGame userId gameAcid = ok $ toResponse $ ("Placeholder." :: Text)

handleRunCommand
    ::  (MonadIO m, Happstack m, Typeable p, Typeable s, Typeable o, SafeCopy p, SafeCopy s, SafeCopy o)
    =>  UserId -> AcidState (GameAcid p s o) -> m Response
handleRunCommand userId gameAcid = ok $ toResponse $ ("Placeholder." :: Text) 

handleGetDisplay
    ::  (MonadIO m, Happstack m, Typeable p, Typeable s, Typeable o, SafeCopy p, SafeCopy s, SafeCopy o)
    =>  UserId -> AcidState (GameAcid p s o) -> m Response
handleGetDisplay userId gameAcid = ok $ toResponse $ ("Placeholder." :: Text)
