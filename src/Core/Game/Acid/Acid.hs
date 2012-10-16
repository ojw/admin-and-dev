{-# LANGUAGE DeriveDataTypeable, GADTs, TemplateHaskell, GeneralizedNewtypeDeriving,
    OverloadedStrings, StandaloneDeriving, TypeFamilies, ScopedTypeVariables #-}

module Core.Game.Acid.Acid where

import Control.Applicative hiding (empty)
import Control.Monad.Reader 
import Data.IxSet
import Data.Acid
import Data.SafeCopy
import Data.Data
import Data.Lens
import Data.Lens.Template
import Data.Text hiding (empty)
import Data.ByteString.Lazy as L hiding (empty)

import Core.Auth.Acid        ( UserId )
import Core.Game.Acid.Lobby
import Core.Game.Acid.Matchmaker
import Core.Game.Acid.Game
import Core.Game.Acid.Location

data Game player state outcome = Game
    { _locationState    :: LocationState
    , _lobbyState       :: LobbyState
    , _defaultLobby     :: Maybe LobbyId
    , _matchmakerState  :: MatchmakerState
    , _gameState        :: GameState player state --outcome
    , _outcome          :: outcome -- placeholder because I have to stop for the night, need to get outcome into state tomorrow
    }

deriving instance (Ord player, Ord state, Ord outcome) => Ord (Game player state outcome)
deriving instance (Eq player, Eq state, Eq outcome) => Eq (Game player state outcome)
deriving instance (Read player, Read state, Read outcome) => Read (Game player state outcome)
deriving instance (Show player, Show state, Show outcome) => Show (Game player state outcome)
deriving instance (Data player, Data state, Data outcome) => Data (Game player state outcome)
deriving instance Typeable3 Game 
