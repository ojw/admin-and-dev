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
import Core.Room.Room
import Core.Auth.Auth
import Core.Location.Acid
import Core.Lobby.Handler
import Core.Lobby.Acid

routeService :: App Response
routeService =
    do  mUserId <- getUserId'
        body <- getBody
        case mUserId of
            Nothing  -> ok $ toResponse ("Not logged in!" :: Text) -- should not be ok
            Just uid -> do  location :: Maybe Game <- query $ Core.Location.Acid.GetLocation uid
                            case fromMaybe Dummy location of
                                Dummy           ->
                                    do  lobby :: Lobby Game <- query GetLobby 
                                        lobbyRouter uid lobby body
