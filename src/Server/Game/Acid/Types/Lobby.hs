{-# LANGUAGE DeriveDataTypeable, GADTs, TemplateHaskell, GeneralizedNewtypeDeriving,
    OverloadedStrings, StandaloneDeriving, TypeFamilies, ScopedTypeVariables #-}

module Server.Game.Acid.Types.Lobby

where

import Control.Applicative hiding ( empty )
import Data.IxSet
import Data.SafeCopy
import Data.Data
import Data.Lens
import Data.Text hiding ( empty )
import Data.Lens.Template

import Server.Auth.Acid        ( UserId )
import Server.Game.Acid.Types.Room  ( RoomId )

newtype LobbyId = LobbyId { _unLobbyId :: Int } deriving (Eq, Ord, Read, Show, Data, Typeable, SafeCopy)

data Lobby = Lobby
    { _lobbyId  :: LobbyId
    , _roomId   :: RoomId
    , _name     :: Text
    } deriving (Ord, Eq, Read, Show, Data, Typeable)

$(makeLens ''Lobby)
$(deriveSafeCopy 0 'base ''Lobby)

instance Indexable Lobby where
    empty = ixSet [ ixFun $ \lobby -> [ lobbyId ^$ lobby ]
                  , ixFun $ \lobby -> [ roomId ^$ lobby ]
                  ]

data LobbyState = LobbyState
    { _nextLobbyId  :: LobbyId
    , _lobbies      :: IxSet Lobby
    } deriving (Ord, Eq, Read, Show, Data, Typeable)

initialLobbyState :: LobbyState
initialLobbyState = LobbyState (LobbyId 1) empty

$(makeLens ''LobbyState)
$(deriveSafeCopy 0 'base ''LobbyState) 