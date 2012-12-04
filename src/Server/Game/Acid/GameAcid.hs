{-# LANGUAGE DeriveDataTypeable, GADTs, TemplateHaskell, GeneralizedNewtypeDeriving,
    OverloadedStrings, StandaloneDeriving, TypeFamilies, ScopedTypeVariables,
    FlexibleContexts, UndecidableInstances #-}

module Server.Game.Acid.GameAcid

( GameAcid(..)
, LocationState(..)
, LobbyState(..)
, LobbyId(..)
, MatchmakerState(..)
, GameState(..)
, locationState, lobbyState, defaultLobby, matchmakerState, gameState
)

where

import Control.Applicative hiding (empty)
import Control.Monad.Reader 
import Data.SafeCopy
import Data.Data
import Data.Lens
import Data.Lens.Template
import Data.Text hiding (empty)
import Data.ByteString.Lazy as L hiding (empty)

import Server.Auth.Acid        ( UserId )
import Server.Game.Acid.Types.Lobby
import Server.Game.Acid.Types.Matchmaker
import Server.Game.Acid.Types.Room
import Server.Game.Acid.Types.Game
import Server.Game.Acid.Types.Location

data GameAcid player state outcome = GameAcid
    { _locationState    :: LocationState
    , _lobbyState       :: LobbyState
    , _defaultLobby     :: Maybe LobbyId
    , _matchmakerState  :: MatchmakerState
    , _gameState        :: GameState player state outcome
    }

$(makeLens ''GameAcid)

deriving instance (Ord player, Ord state, Ord outcome, Typeable player, Typeable state, Typeable outcome) => Ord (GameAcid player state outcome)
deriving instance (Eq player, Eq state, Eq outcome, Ord player, Ord state, Ord outcome, Typeable player, Typeable state, Typeable outcome) => Eq (GameAcid player state outcome)
deriving instance (Read player, Read state, Read outcome, Ord player, Ord state, Ord outcome, Typeable player, Typeable state, Typeable outcome) => Read (GameAcid player state outcome)
deriving instance (Show player, Show state, Show outcome, Ord player, Ord state, Ord outcome, Typeable player, Typeable state, Typeable outcome) => Show (GameAcid player state outcome)
deriving instance (Data player, Data state, Data outcome, Ord player, Ord state, Ord outcome) => Data (GameAcid player state outcome)
deriving instance Typeable3 GameAcid

instance (SafeCopy (GameState player state outcome)) => SafeCopy (GameAcid player state outcome) where
    putCopy (GameAcid locationState lobbyState defaultLobby matchmakerState gameState) = 
        contain $ do safePut locationState; safePut lobbyState; safePut defaultLobby; safePut matchmakerState; safePut gameState
    getCopy = contain $ GameAcid <$> safeGet <*> safeGet <*> safeGet <*> safeGet <*> safeGet
