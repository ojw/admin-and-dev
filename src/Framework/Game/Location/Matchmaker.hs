{-# LANGUAGE GeneralizedNewtypeDeriving, DeriveDataTypeable, TemplateHaskell #-}

module Framework.Game.Location.Matchmaker where

import Data.SafeCopy
import Data.Data
import Data.Lens.Template
import Data.IxSet

import Framework.Auth.Auth              ( UserId )
import Framework.Game.Location.Lobby
import Framework.Game.Location.Chat

newtype MatchmakerId = MatchmakerId Int deriving (Ord, Eq, Read, Show, Data, Typeable, SafeCopy, Enum)

data Matchmaker = Matchmaker
    { _matchmakerId  :: MatchmakerId
    , _chat          :: ChatHolder
    , _capacity      :: (Int, Int) -- (min, max)
    , _owner         :: UserId
    , _lobbyId       :: LobbyId
    } deriving (Ord, Eq, Read, Show, Data, Typeable)

$(makeLens ''Matchmaker)
$(deriveSafeCopy 0 'base ''Matchmaker)
$(inferIxSet "Matchmakers" ''Matchmaker 'noCalcs [''MatchmakerId, ''UserId, ''LobbyId])

data MatchmakerState = MatchmakerState
    { _matchmakers      :: Matchmakers
    , _nextMatchmakerId :: MatchmakerId
    } deriving (Ord, Eq, Read, Show, Data, Typeable)

$(makeLens ''MatchmakerState)
$(deriveSafeCopy 0 'base ''MatchmakerState)
