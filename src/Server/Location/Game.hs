{-# LANGUAGE TemplateHaskell, DeriveDataTypeable, GeneralizedNewtypeDeriving #-}

module Server.Location.Game where

import Data.IxSet
import Data.SafeCopy
import Data.Data
import Data.Lens
import Data.Lens.Template

import Server.Auth.Acid         ( UserId )
import Server.Location.Lobby    ( LobbyId )
import Server.Location.Chat -- ( ChatList, ChatRoom )

newtype GameId = GameId { _unGame :: Int } deriving (Ord, Eq, Read, Show, Data, Typeable, SafeCopy, Enum)

data Game = Game
    { _lobbyId   :: LobbyId
    , _chatList  :: ChatList
    , _gameId    :: GameId
    } deriving (Ord, Eq, Read, Show, Data, Typeable)

$(makeLens ''Game)
$(deriveSafeCopy 0 'base ''Game)

instance Indexable Game where
    empty = ixSet [ ixFun $ \game -> [ gameId ^$ game ]
                  ]

data GameState = GameState
    { _nextGameId   :: GameId
    , _games        :: IxSet Game
    } deriving (Ord, Eq, Read, Show, Data, Typeable)

$(makeLens ''GameState)
$(deriveSafeCopy 0 'base ''GameState)
