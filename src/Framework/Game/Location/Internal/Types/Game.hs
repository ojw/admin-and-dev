{-# LANGUAGE GeneralizedNewtypeDeriving, DeriveDataTypeable, TemplateHaskell #-}

module Framework.Game.Location.Internal.Types.Game where

import Data.SafeCopy
import Data.Data
import Data.Lens.Template
import Data.IxSet

import Framework.Game.Location.Internal.Types.Chat
import Framework.Game.Location.Internal.Types.Lobby        ( LobbyId )
import Framework.Game.Location.Internal.Types.Matchmaker   ( MatchmakerId )

newtype GameId = GameId Int deriving (Ord, Eq, Read, Show, Data, Typeable, SafeCopy, Enum)

data Game = Game
    { _gameId        :: GameId
    , _matchmakerId  :: MatchmakerId
    , _lobbyId       :: LobbyId
    , _chats         :: ChatHolder
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
