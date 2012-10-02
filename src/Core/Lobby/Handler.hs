{-# LANGUAGE DeriveDataTypeable, GADTs, TemplateHaskell, GeneralizedNewtypeDeriving,
    OverloadedStrings, StandaloneDeriving, TypeFamilies, ScopedTypeVariables,
    FlexibleContexts #-}

module Core.Lobby.Handler

where

import Control.Applicative hiding (empty)
import Control.Monad.Reader 
import Data.IxSet
import Data.Acid hiding ( query )
import Data.Acid.Advanced
import Data.SafeCopy
import Data.Data
import Data.Lens
import Data.Lens.Template
import Data.Aeson
import Data.Text hiding (empty)
import Data.ByteString.Lazy as L hiding (empty)
import Happstack.Server

import Core.Auth.Acid        ( UserId, AuthState, ProfileState )
import Core.Room.Acid        ( RoomId, RoomState )
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

lobbyRouter 
    :: (Happstack m, HasAcidState m RoomState, HasAcidState m AuthState, HasAcidState m ProfileState, HasAcidState m (Lobby game), Typeable game, SafeCopy game) 
    =>  UserId -> AcidState (Lobby game) -> ByteString -> m Response
lobbyRouter userId acidLobby body =
    do  location <- query' acidLobby (GetLocation userId)
        --lobby <- query' acidLobby (GetLobby)
        case location of
            InLobby         -> lobbyRequestHandler userId acidLobby body
            InGame          -> ok $ toResponse $ ("Game functions are in the works." :: Text)
            InMatchmaker    -> ok $ toResponse $ ("Matchmaker functions are in the works." :: Text)

lobbyRequestHandler 
    :: (Happstack m, HasAcidState m RoomState, HasAcidState m AuthState, HasAcidState m ProfileState, HasAcidState m (Lobby game), Typeable game, SafeCopy game) 
    =>  UserId -> AcidState (Lobby game) -> ByteString -> m Response
lobbyRequestHandler userId acidLobby body =
        case getDomain body of -- this is inefficient -- I believe that it causes the body to be parsed twice
            Nothing             -> ok $ toResponse ("Bad json." :: Text) -- should not be ok
            Just DomRoom        -> do roomId <- query' acidLobby GetRoomId; processRoomRequest userId roomId body
            Just DomLobby       -> ok $ toResponse ("Haven't added this domain yet." :: Text)
            Just DomGame        -> ok $ toResponse ("Game requests when user is InLobby don't make sense." :: Text)
            Just DomMatchmaker  -> ok $ toResponse ("Matchmaker requests when user is InLobby don't make sense" :: Text)
