{-# LANGUAGE DeriveDataTypeable, GADTs, TemplateHaskell, GeneralizedNewtypeDeriving,
    OverloadedStrings, StandaloneDeriving, TypeFamilies, ScopedTypeVariables,
    FlexibleContexts #-}

module Core.Lobby.Handler

where

import Control.Applicative hiding (empty)
import Control.Monad.Reader 
import Data.IxSet
import Data.Acid
import Data.SafeCopy
import Data.Data
import Data.Lens
import Data.Lens.Template
import Data.Aeson
import Data.Text hiding (empty)
import Data.ByteString.Lazy as L hiding (empty)
import Happstack.Server

import Core.Auth.Acid        ( UserId, AuthState, ProfileState )
import Core.Room.Acid.Core   ( RoomId, RoomState )
import Core.Room.Api
import Util.HasAcidState
import Util.GetBody
import Core.Lobby.Acid

data Domain = DomRoom | DomLobby | DomGame | DomMatchmaker

instance FromJSON Domain where
    parseJSON (Object o) =
        do  domain <- o .: "domain"
            case domain of
                ("room" :: Text)  -> return DomRoom
                _       -> mzero
    parseJSON _ = mzero

getDomain :: ByteString -> Maybe Domain
getDomain = decode

routeService :: (Happstack m, HasAcidState m RoomState, HasAcidState m AuthState, HasAcidState m ProfileState) =>  UserId -> Lobby game -> ByteString -> m Response
routeService userId lobby body=
        case getDomain body of -- this is inefficient -- I believe that it causes the body to be parsed twice
            Nothing             -> ok $ toResponse ("Bad json." :: Text) -- should not be ok
            Just DomRoom        -> processRoomRequest userId body
            Just DomLobby       -> ok $ toResponse ("Haven't added this domain yet." :: Text)
            Just DomGame        -> ok $ toResponse ("Haven't added this domain yet." :: Text)
            Just DomMatchmaker  -> ok $ toResponse ("Haven't added this domain yet." :: Text)
