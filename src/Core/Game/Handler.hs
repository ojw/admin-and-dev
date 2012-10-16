{-# LANGUAGE DeriveDataTypeable, GADTs, TemplateHaskell, GeneralizedNewtypeDeriving,
    OverloadedStrings, StandaloneDeriving, TypeFamilies, ScopedTypeVariables,
    FlexibleContexts #-}

module Core.Game.Handler

where

import Control.Applicative hiding (empty)
import Control.Monad.Reader 
import Data.Maybe       ( fromJust )
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
import Core.Lobby.Acid
import Core.Game.Acid
import Core.Room.Api
import Util.HasAcidState
import Util.GetBody
import Core.Lobby.Acid
import Core.Location.Acid

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

gameRouter 
    ::  (Happstack m, HasAcidState m RoomState, Ord game, Typeable game, SafeCopy game)
    =>  UserId -> Maybe Location -> AcidState GameHolder -> AcidState (LobbyState game) -> ByteString -> m Response
gameRouter userId location acidGame lobbyState body =
    do  --location <- query' acidGame (GetLocation userId)
        --lobby <- query' acidGame (GetLobby)
        case location of
            Just (InLobby lobbyId)  -> lobbyRequestHandler userId lobbyId lobbyState body
            Just (InGame gameId)    -> ok $ toResponse $ ("Game functions are in the works." :: Text)
            Just (InMatchmaker mId) -> ok $ toResponse $ ("Matchmaker functions are in the works." :: Text)
            Nothing                 -> ok $ toResponse $ ("You are not in a location :/" :: Text)

lobbyRequestHandler 
    :: (Happstack m, HasAcidState m RoomState, Ord game, Typeable game, SafeCopy game)
    =>  UserId -> LobbyId -> AcidState (LobbyState game) -> ByteString -> m Response
lobbyRequestHandler userId lobbyId lobbyState body =
        case getDomain body of -- this is inefficient -- I believe that it causes the body to be parsed twice
            Nothing             -> ok $ toResponse ("Bad json." :: Text) -- should not be ok
            Just DomRoom        -> do   roomId <- fromJust <$> query' lobbyState (GetRoomId lobbyId)
                                        processRoomRequest userId roomId body
            Just DomLobby       -> ok $ toResponse ("Haven't added this domain yet." :: Text)
            Just DomGame        -> ok $ toResponse ("Game requests when user is InLobby don't make sense." :: Text)
            Just DomMatchmaker  -> ok $ toResponse ("Matchmaker requests when user is InLobby don't make sense" :: Text)
