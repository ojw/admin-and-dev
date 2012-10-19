{-# LANGUAGE TemplateHaskell, DeriveDataTypeable, GeneralizedNewtypeDeriving, TypeFamilies #-}

module Core.Game.Acid.Types.Matchmaker where

import Data.Data
import Data.Functor         ( (<$>) )
import Data.Acid
import Data.SafeCopy
import Control.Monad.State  ( get )
import Control.Monad.Reader ( ask )
import Data.IxSet
import Data.Lens
import Data.Lens.Template

import Core.Auth.Acid               ( UserId )
import Core.Game.Acid.Types.Room    ( RoomId )
import Core.Game.Acid.Types.Lobby   ( LobbyId )

newtype MatchmakerId = MatchmakerId { _unMatchmakerId :: Int } deriving (Ord, Eq, Read, Show, Data, Typeable, Enum, SafeCopy)
$(makeLens ''MatchmakerId)

data Matchmaker = Matchmaker
    { _matchmakerId :: MatchmakerId
    , _capacity     :: Int
    , _required     :: Int
    , _owner        :: UserId
    , _roomId       :: RoomId
    , _lobbyId      :: LobbyId -- the lobby that spawned the matchmaker, where to kick players if it closes
    } deriving (Ord, Eq, Read, Show, Data, Typeable)

$(makeLens ''Matchmaker)
$(deriveSafeCopy 0 'base ''Matchmaker)

newtype Owner = Owner { _unOwner :: UserId } deriving (Ord, Eq, Read, Show, Data, Typeable)

instance Indexable Matchmaker where
    empty = ixSet [ ixFun $ \matchmaker -> [ matchmakerId ^$ matchmaker ]
                  , ixFun $ \matchmaker -> [ capacity ^$ matchmaker ]
                  , ixFun $ \matchmaker -> [ Owner $ owner ^$ matchmaker ]
                  ]

data MatchmakerState = MatchmakerState
    { _nextMatchmaker   :: MatchmakerId
    , _matchmakers      :: IxSet Matchmaker
    } deriving (Ord, Eq, Read, Show, Data, Typeable)

initialMatchmakerState :: MatchmakerState
initialMatchmakerState = MatchmakerState (MatchmakerId 1) empty

$(makeLens ''MatchmakerState)
$(deriveSafeCopy 0 'base ''MatchmakerState)
