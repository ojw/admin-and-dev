{-# LANGUAGE DeriveDataTypeable, GADTs, TemplateHaskell, GeneralizedNewtypeDeriving #-}

module Core.Lobby.Acid

where

import Data.IxSet
import Data.Acid
import Data.SafeCopy
import Data.Data
import Data.Lens
import Data.Lens.Template

import Core.Auth.Acid        ( UserId )
import Core.Room.Acid.Core   ( RoomId )

import Util.HasAcidState

-- gonna start with Lobby as a container with a room...
-- then add permissions, matchmaking, and games

newtype LobbyId = LobbyId { _unLobbyId :: Int } deriving (Ord, Eq, Data, Typeable, Read, Show, SafeCopy)

data SubLocation = InLobby | InMatchmaker | InGame -- ugh, the name is bad
    deriving (Ord, Eq, Read, Show, Data, Typeable)

$(deriveSafeCopy 0 'base ''SubLocation)

data UserLocation = UserLocation 
    { _userId        :: UserId
    , _subLocation   :: SubLocation
    } deriving (Ord, Eq, Data, Typeable, Read, Show)

$(makeLens ''UserLocation)

$(deriveSafeCopy 0 'base ''UserLocation)

instance Indexable UserLocation where
    empty = ixSet [ ixFun $ \location -> [ userId ^$ location ]
                  , ixFun $ \location -> [ subLocation ^$ location ]
                  ]

data Lobby = Lobby
    { _lobbyId       :: LobbyId
    , _roomId        :: RoomId
    , _userLocations :: IxSet UserLocation
    } deriving (Ord, Eq, Data, Typeable, Read, Show)

$(makeLens ''Lobby)
$(deriveSafeCopy 0 'base ''Lobby)

instance Indexable Lobby where
    empty = ixSet [ ixFun $ \lobby -> [ roomId ^$ lobby ]
                  ]

data LobbyState = LobbyState
    { _nextLobbyId   :: LobbyId
    , _lobbies       :: IxSet Lobby
    } deriving (Ord, Eq, Data, Typeable, Read, Show)

$(makeLens ''LobbyState)
$(deriveSafeCopy 0 'base ''LobbyState)

$(makeAcidic ''LobbyState [])
