{-# LANGUAGE GeneralizedNewtypeDeriving, DeriveDataTypeable, TemplateHaskell #-}

module Framework.Game.Location.Lobby where

import Data.SafeCopy
import Data.Data
import Data.Lens.Template
import Data.IxSet

import Framework.Game.Location.Chat     ( ChatHolder )

newtype LobbyId = LobbyId Int deriving (Ord, Eq, Read, Show, Data, Typeable, SafeCopy, Enum)

data Lobby = Lobby
    { _lobbyId   :: LobbyId
    , _chat      :: ChatHolder
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
