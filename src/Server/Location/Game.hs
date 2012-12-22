{-# LANGUAGE TemplateHaskell, DeriveDataTypeable, GeneralizedNewtypeDeriving #-}

module Server.Location.Game where

import Data.IxSet
import Data.SafeCopy
import Data.Data
import Data.Lens
import Data.Lens.Template

import Server.Auth.Acid         ( UserId(..) )
import Server.Location.Lobby    ( LobbyId(..) )
import Server.Location.Chat -- ( ChatList, ChatRoom )
import Server.Location.Matchmaker ( MatchmakerId(..) )

newtype GameId = GameId { _unGame :: Int } deriving (Ord, Eq, Read, Show, Data, Typeable, SafeCopy, Enum)

data Game = Game
    { _lobbyId      :: LobbyId
    , _matchmakerId :: MatchmakerId
    , _chatList     :: ChatList
    , _gameId       :: GameId
    , _owner        :: UserId
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

instance ChatRoom Game where
    addChat chat game = game { _chatList = addChat chat (_chatList game) }
    getChats game = getChats (_chatList game)

stupidEmptyGame :: Game
stupidEmptyGame = Game (LobbyId 1) (MatchmakerId 1) emptyChatList (GameId 1) (UserId 1)

$(makeLens ''GameState)
$(deriveSafeCopy 0 'base ''GameState)
