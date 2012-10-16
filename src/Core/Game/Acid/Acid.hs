{-# LANGUAGE DeriveDataTypeable, GADTs, TemplateHaskell, GeneralizedNewtypeDeriving,
    OverloadedStrings, StandaloneDeriving, TypeFamilies, ScopedTypeVariables,
    FlexibleContexts #-}

module Core.Game.Acid.Acid where

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
import Core.Game.Acid.Types.Game
import Core.Game.Acid.Types.Location

data GameAcid player state outcome = GameAcid
    { _locationState    :: LocationState
    , _lobbyState       :: LobbyState
    , _defaultLobby     :: Maybe LobbyId
    , _matchmakerState  :: MatchmakerState
    , _gameState        :: GameState player state --outcome
    , _outcome          :: outcome -- placeholder because I have to stop for the night, need to get outcome into state tomorrow
    }

$(makeLens ''GameAcid)

deriving instance (Ord player, Ord state, Ord outcome) => Ord (GameAcid player state outcome)
deriving instance (Eq player, Eq state, Eq outcome) => Eq (GameAcid player state outcome)
deriving instance (Read player, Read state, Read outcome) => Read (GameAcid player state outcome)
deriving instance (Show player, Show state, Show outcome) => Show (GameAcid player state outcome)
deriving instance (Data player, Data state, Data outcome) => Data (GameAcid player state outcome)
deriving instance Typeable3 GameAcid

instance (SafeCopy (GameState player state), SafeCopy outcome) => SafeCopy (GameAcid player state outcome) where
    putCopy (GameAcid locationState lobbyState defaultLobby matchmakerState gameState outcome) = 
        contain $ do safePut locationState; safePut lobbyState; safePut defaultLobby; safePut matchmakerState; safePut gameState; safePut outcome
    getCopy = contain $ GameAcid <$> safeGet <*> safeGet <*> safeGet <*> safeGet <*> safeGet <*> safeGet
