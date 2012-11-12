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
import Data.Acid.Advanced

import Server.Room.Api
import Util.GetBody
import Acid
import App
import Server.Room.Acid
import Server.Auth.Auth
import Server.Location.Acid
import Server.Lobby.Acid
import Server.GameHolder.Handler
import Server.GameHolder.Acid

routeService :: App Response
routeService =
    do  mUserId <- getUserId'
        body <- getBody
        case mUserId of
            Nothing  -> ok $ toResponse ("Not logged in!" :: Text) -- should not be ok
            Just uid -> do  locationState :: AcidState (LocationState Games) <- getAcidState
                            lobbyState :: AcidState (LobbyState Games) <- getAcidState
                            game :: Maybe Games <- query' locationState $  Server.Location.Acid.GetGame uid
                            location :: Maybe Location <- query' locationState $ Server.Location.Acid.GetLocation uid
                            case fromMaybe Dummy game of -- should remove fromMaybe; Nothing means player is at game selecting menu
                                Dummy           ->
                                    do  gameAcid :: AcidState GameHolder <- getAcidState
                                        gameRouter uid location gameAcid lobbyState body
