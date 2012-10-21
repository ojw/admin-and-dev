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

import Util.HasAcidState
import Core.Profile.Acid as Profile
import Core.Auth.Acid        ( UserId )
import Core.Game.Api.Room
import Core.Game.Api.Lobby
import Core.Game.Api.Matchmaker
import Core.Game.Api.Game
import Core.Game.Acid.GameAcid

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
    ::  (Happstack m, SafeCopy p, SafeCopy s, SafeCopy o, Typeable p, Typeable s, Typeable o, Ord p, Ord s, Ord o, HasAcidState m Profile.ProfileState)
    =>  UserId -> AcidState (GameAcid p s o) -> ByteString -> m Response
gameRouter userId gameAcid body =
    case getDomain body of
        Just DomRoom        -> processRoomRequest userId gameAcid body
        Just DomLobby       -> processLobbyRequest userId gameAcid body
        Just DomMatchmaker  -> processMatchmakerRequest userId gameAcid body
        Just DomGame        -> processGameRequest userId gameAcid body
        Nothing             -> ok $ toResponse $ ("Request lacks domain." :: Text)
