{-# LANGUAGE GeneralizedNewtypeDeriving, DeriveDataTypeable, TemplateHaskell #-}

module Framework.Game.Location.Game where

import Data.SafeCopy
import Data.Data
import Data.Lens.Template
import Data.IxSet

import Framework.Game.Location.Chat
import Framework.Game.Location.Lobby        ( LobbyId )
import Framework.Game.Location.Matchmaker   ( MatchmakerId )

newtype GameId = GameId Int deriving (Ord, Eq, Read, Show, Data, Typeable, SafeCopy, Enum)

data Game = Game
    { _gameId        :: GameId
    , _matchmakerId  :: MatchmakerId
    , _lobbyId       :: LobbyId
    , _chat          :: ChatHolder
    } deriving (Ord, Eq, Read, Show, Data, Typeable)

$(makeLens ''Game)
$(deriveSafeCopy 0 'base ''Game)
$(inferIxSet "Games" ''Game 'noCalcs [''GameId, ''MatchmakerId, ''LobbyId])

data GameState = GameState
    { _games        :: Games
    , _nextGameId   :: GameId
    } deriving (Ord, Eq, Read, Show, Data, Typeable)

$(makeLens ''GameState)
$(deriveSafeCopy 0 'base ''GameState)
