{-# LANGUAGE Rank2Types, OverloadedStrings, ScopedTypeVariables #-}

module Service where

import Control.Monad.Trans
import Control.Monad
import Util.HasAcidState
import Data.ByteString.Lazy
import Happstack.Server
import Data.Aeson
import Data.Aeson.Types
import Data.Text
import Data.Maybe ( fromMaybe )
import Data.Acid hiding (query)

import Core.Room.Api
import Util.GetBody
import Acid
import App
import Core.Room.Acid
import Core.Auth.Auth
import Core.Location.Acid
import Core.Lobby.Acid
import Core.GameHolder.Handler
import Core.GameHolder.Acid

routeService :: App Response
routeService =
    do  mUserId <- getUserId'
        body <- getBody
        case mUserId of
            Nothing  -> ok $ toResponse ("Not logged in!" :: Text) -- should not be ok
            Just uid -> do  location :: Maybe Games <- query $ Core.Location.Acid.GetLocation uid
                            case fromMaybe Dummy location of
                                Dummy           ->
                                    do  gameAcid :: AcidState GameHolder <- getAcidState
                                        gameRouter uid gameAcid body
