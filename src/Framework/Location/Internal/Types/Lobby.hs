{-# LANGUAGE GeneralizedNewtypeDeriving, DeriveDataTypeable, TemplateHaskell #-}

module Framework.Location.Internal.Types.Lobby where

import Data.SafeCopy
import Data.Text
import Data.Data
import Data.Lens.Template
import Data.IxSet

import Framework.Location.Internal.Types.Chat     ( ChatHolder )

newtype LobbyId = LobbyId Int deriving (Ord, Eq, Read, Show, Data, Typeable, SafeCopy, Enum)

data Lobby = Lobby
    { _name         :: Text
    , _description  :: Text
    , _lobbyId      :: LobbyId
    , _chats        :: ChatHolder
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
