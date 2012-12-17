{-# LANGUAGE DeriveDataTypeable, GADTs, TemplateHaskell, GeneralizedNewtypeDeriving,
    OverloadedStrings, StandaloneDeriving, TypeFamilies, ScopedTypeVariables #-}

module Server.Location.Lobby

where

import Control.Applicative hiding ( empty )
import Data.IxSet
import Data.SafeCopy
import Data.Data
import Data.Lens
import Data.Text hiding ( empty )
import Data.Lens.Template

import Server.Auth.Acid        ( UserId )
import Server.Location.Chat -- ( ChatList, ChatRoom )

newtype LobbyId = LobbyId { _unLobbyId :: Int } deriving (Eq, Ord, Read, Show, Data, Typeable, SafeCopy, Enum)

data Lobby = Lobby
    { _lobbyId  :: LobbyId
    , _chatList :: ChatList
    , _name     :: Text
    } deriving (Ord, Eq, Read, Show, Data, Typeable)

type LobbyView = Lobby

$(makeLens ''Lobby)
$(deriveSafeCopy 0 'base ''Lobby)

instance ChatRoom Lobby where
    addChat chat = chatList ^%= addChat chat
    getChats = getChats . _chatList

instance Indexable Lobby where
    empty = ixSet [ ixFun $ \lobby -> [ lobbyId ^$ lobby ]
                  ]

data LobbyState = LobbyState
    { _nextLobbyId  :: LobbyId
    , _lobbies      :: IxSet Lobby
    } deriving (Ord, Eq, Read, Show, Data, Typeable)

initialLobbyState :: LobbyState
initialLobbyState = LobbyState (LobbyId 1) empty

$(makeLens ''LobbyState)
$(deriveSafeCopy 0 'base ''LobbyState) 
