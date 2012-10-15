{-# LANGUAGE DeriveDataTypeable, GADTs, TemplateHaskell, GeneralizedNewtypeDeriving,
    OverloadedStrings, StandaloneDeriving, TypeFamilies, ScopedTypeVariables #-}

module Core.GameHolder.Acid where

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
import Core.GameHolder.Lobby
import Core.GameHolder.Matchmaker

newtype GameId = GameId Int deriving (Ord, Eq, Read, Show, Data, Typeable, SafeCopy)

data Location = InLobby LobbyId | InMatchmaker MatchmakerId | InGame GameId
    deriving (Ord, Eq, Read, Show, Data, Typeable)

$(deriveSafeCopy 0 'base ''Location)

data UserLocation = UserLocation
    { _userId   :: UserId
    , _location :: Maybe Location
    } deriving (Ord, Eq, Read, Show, Data, Typeable)

$(makeLens ''UserLocation)
$(deriveSafeCopy 0 'base ''UserLocation)

instance Indexable UserLocation where
    empty = ixSet [ ixFun $ \location -> [ userId ^$ location ]
                  , ixFun $ \location -> [ _location location ]
                  ]

newtype LocationState = LocationState { _unLocationState :: IxSet UserLocation }
    deriving (Ord, Eq, Read, Show, Data, Typeable, SafeCopy)

data GameState player state outcome = GameState
    { _gameId :: GameId
    , _state :: Either state outcome
    , _players :: [(UserId,player)]
    }

deriving instance (Ord player, Ord state, Ord outcome) => Ord (GameState player state outcome)
deriving instance (Eq player, Eq state, Eq outcome) => Eq (GameState player state outcome)
deriving instance (Read player, Read state, Read outcome) => Read (GameState player state outcome)
deriving instance (Show player, Show state, Show outcome) => Show (GameState player state outcome)
deriving instance (Data player, Data state, Data outcome) => Data (GameState player state outcome)
deriving instance Typeable3 GameState

data GameHolder player state outcome = GameHolder
    { _locationState    :: LocationState
    , _lobbyState       :: LobbyState
    , _defaultLobby     :: Maybe LobbyId
    , _matchmakerState  :: MatchmakerState
    , _gameState        :: GameState player state outcome
    }

deriving instance (Ord player, Ord state, Ord outcome) => Ord (GameHolder player state outcome)
deriving instance (Eq player, Eq state, Eq outcome) => Eq (GameHolder player state outcome)
deriving instance (Read player, Read state, Read outcome) => Read (GameHolder player state outcome)
deriving instance (Show player, Show state, Show outcome) => Show (GameHolder player state outcome)
deriving instance (Data player, Data state, Data outcome) => Data (GameHolder player state outcome)
deriving instance Typeable3 GameHolder 
