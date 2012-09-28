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

data Domain = Room | Lobby | Game | Matchmaker

instance FromJSON Domain where
    parseJSON (Object o) =
        do  domain <- o .: "domain"
            case domain of
                ("room" :: Text)  -> return Room
                _       -> mzero
    parseJSON _ = mzero

getDomain :: ByteString -> Maybe Domain
getDomain = decode

routeService :: App Response
routeService =
    do  mUserId <- getUserId'
        body <- getBody
        case mUserId of
            Nothing  -> ok $ toResponse ("Not logged in!" :: Text) -- should not be ok
            Just uid ->
                case getDomain body of -- this is inefficient -- I believe that it causes the body to be parsed twice
                    Nothing         -> ok $ toResponse ("Bad json." :: Text) -- should not be ok
                    Just Room       -> processRoomRequest uid body
                    Just Lobby      -> ok $ toResponse ("Haven't added this domain yet." :: Text)
                    Just Game       -> ok $ toResponse ("Haven't added this domain yet." :: Text)
                    Just Matchmaker -> ok $ toResponse ("Haven't added this domain yet." :: Text)
