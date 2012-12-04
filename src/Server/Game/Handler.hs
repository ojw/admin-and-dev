{-# LANGUAGE DeriveDataTypeable, GADTs, TemplateHaskell, GeneralizedNewtypeDeriving,
    OverloadedStrings, StandaloneDeriving, TypeFamilies, ScopedTypeVariables,
    FlexibleContexts #-}

module Server.Game.Handler

where

import Control.Monad ( mzero )
import Data.Acid
import Data.SafeCopy
import Data.Data
import Data.Aeson
import Data.Text
import Data.ByteString.Lazy as L
import Happstack.Server

import Util.HasAcidState
import Server.Profile.Acid as Profile
import Server.Auth.Acid        ( UserId )
import Server.Game.Api.Matchmaker
import Server.Game.Api.Lobby
import Server.Game.Api.Game
import Server.Game.Acid.GameAcid

data Domain = DomLobby | DomGame | DomMatchmaker

instance FromJSON Domain where
    parseJSON (Object o) =
        do  domain <- o .: "domain"
            case domain of
                ("lobby" :: Text)       -> return DomLobby
                ("game" :: Text)        -> return DomGame
                ("matchmaker" :: Text)  -> return DomMatchmaker
                _       -> mzero
    parseJSON _ = mzero

getDomain :: ByteString -> Maybe Domain
getDomain = decode

gameRouter 
    ::  (Happstack m, SafeCopy p, SafeCopy s, SafeCopy o, Typeable p, Typeable s, Typeable o, Ord p, Ord s, Ord o, HasAcidState m Profile.ProfileState)
    =>  UserId -> AcidState (GameAcid p s o) -> ByteString -> m Response
gameRouter userId gameAcid body =
    case getDomain body of
        Just DomLobby       -> processLobbyRequest userId gameAcid body
        Just DomMatchmaker  -> processMatchmakerRequest userId gameAcid body
        Just DomGame        -> processGameRequest userId gameAcid body
        Nothing             -> ok $ toResponse $ ("Request lacks domain." :: Text)
