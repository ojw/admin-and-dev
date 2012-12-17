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

type GameState = Game

$(makeLens ''Game)
$(deriveSafeCopy 0 'base ''Game)
