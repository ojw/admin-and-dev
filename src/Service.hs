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

import Core.Auth
import Core.Room.Api
import Util.GetBody
import Acid
import App
import Core.Room
import Core.Auth

-- It can store some stuff, and it can use that stuff to provide a response
data StatefulService state = StatefulService
    { initialState  :: state -- what we stick in the Acid type
    , handler       :: (HasAcidState m state) => ByteString -> m Response -- takes ByteString, probably parses into json, does some stuff and returns a response
    }

-- could have
-- authService :: StatefulService AuthState
-- profileService :: StatefulService Profile State
-- ...

-- acid will be initialized with (initialState authService), etc
-- handler will call (handler authService), etc

data Domain = Room | Lobby | Game | Matchmaker

instance FromJSON Domain where
    parseJSON (Object o) =
        do  domain <- o .: "domain"
            case domain of
                ("room" :: Text)  -> return Room
                _       -> mzero
    parseJSON _ = mzero

getDomain json = decode json :: Maybe Domain

-- routeService :: App Response
routeService :: App Response -- (HasAcidState m RoomState, HasAcidState m AuthState, HasAcidState m ProfileState, Happstack m, MonadIO m) => m Response
routeService =
    do 
        mUserId <- getUserId'
        body <- getBody
        case mUserId of
            Nothing  -> ok $ toResponse ("Not logged in!" :: Text) -- should not be ok
            Just uid -> 
                case getDomain body of
                    Nothing     -> ok $ toResponse ("Bad json." :: Text) -- should not be ok
                    Just Room   -> processRoomRequest uid body 
                    Just _      -> ok $ toResponse ("Haven't added this domain yet." :: Text)
        
