{-# LANGUAGE GeneralizedNewtypeDeriving, DeriveDataTypeable, TemplateHaskell #-}

module Framework.Game.Location.Internal.Types.Lobby where

import Data.SafeCopy
import Data.Data
import Data.Lens.Template
import Data.IxSet

import Framework.Game.Location.Internal.Types.Chat     ( ChatHolder )

newtype LobbyId = LobbyId Int deriving (Ord, Eq, Read, Show, Data, Typeable, SafeCopy, Enum)

data Lobby = Lobby
    { _lobbyId   :: LobbyId
    , _chats     :: ChatHolder
    } deriving (Ord, Eq, Read, Show, Data, Typeable)

$(makeLens ''Lobby)
$(deriveSafeCopy 0 'base ''Lobby)
$(inferIxSet "Lobbies" ''Lobby 'noCalcs [''LobbyId])

data LobbyState = LobbyState
    { _lobbies       :: Lobbies
    , _nextLobbyId   :: LobbyId
    } deriving (Ord, Eq, Read, Show, Data, Typeable)

$(makeLens ''LobbyState)
$(deriveSafeCopy 0 'base ''LobbyState)
