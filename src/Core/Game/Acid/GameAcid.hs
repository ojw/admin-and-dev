{-# LANGUAGE DeriveDataTypeable, GADTs, TemplateHaskell, GeneralizedNewtypeDeriving,
    OverloadedStrings, StandaloneDeriving, TypeFamilies, ScopedTypeVariables,
    FlexibleContexts #-}

module Core.Game.Acid.GameAcid

( GameAcid(..)
, LocationState(..)
, LobbyState(..)
, LobbyId(..)
, MatchmakerState(..)
, GameState(..)
, locationState, lobbyState, defaultLobby, matchmakerState, roomState, gameState, outcome
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

import Core.Auth.Acid        ( UserId )
import Core.Game.Acid.Types.Lobby
import Core.Game.Acid.Types.Matchmaker
import Core.Game.Acid.Types.Room
import Core.Game.Acid.Types.Game
import Core.Game.Acid.Types.Location

data GameAcid player state outcome = GameAcid
    { _locationState    :: LocationState
    , _lobbyState       :: LobbyState
    , _defaultLobby     :: Maybe LobbyId
    , _matchmakerState  :: MatchmakerState
    , _roomState        :: RoomState
    , _gameState        :: GameState player state --outcome
    , _outcome          :: outcome -- placeholder
    }

$(makeLens ''GameAcid)

deriving instance (Ord player, Ord state, Ord outcome, Typeable player, Typeable state) => Ord (GameAcid player state outcome)
deriving instance (Eq player, Eq state, Eq outcome, Ord player, Ord state, Typeable player, Typeable state) => Eq (GameAcid player state outcome)
deriving instance (Read player, Read state, Read outcome, Ord player, Ord state, Typeable player, Typeable state) => Read (GameAcid player state outcome)
deriving instance (Show player, Show state, Show outcome, Ord player, Ord state, Typeable player, Typeable state) => Show (GameAcid player state outcome)
deriving instance (Data player, Data state, Data outcome, Ord player, Ord state) => Data (GameAcid player state outcome)
deriving instance Typeable3 GameAcid

instance (SafeCopy (GameState player state), SafeCopy outcome) => SafeCopy (GameAcid player state outcome) where
    putCopy (GameAcid locationState lobbyState defaultLobby matchmakerState roomState gameState outcome) = 
        contain $ do safePut locationState; safePut lobbyState; safePut defaultLobby; safePut matchmakerState; safePut roomState; safePut gameState; safePut outcome
    getCopy = contain $ GameAcid <$> safeGet <*> safeGet <*> safeGet <*> safeGet <*> safeGet <*> safeGet <*> safeGet
