{-# LANGUAGE DeriveDataTypeable #-}
-- apparently DatatypeContexts is a misfeature
-- what should I use instead?

module Plugins.Lobby.Acid

where

import Data.IxSet               ( IxSet )
import Data.Acid
import Data.Data

import Plugins.Auth.Acid        ( UserId )
import Plugins.Room.Acid.Core   ( RoomId )

import Util.HasAcidState

-- state is type of game state to be stored during gameplay
-- options is type of all options (including players joining game) to store before game starts
-- permissions is datatype that game will translate to bool for players trying to join lobby
-- possibly have OngoingGame and OpenGame types similarly parameterized in Plugins.Game for use here
--
-- might need to have class constraint on a parameter! this might be getting tricky...
--
-- gonna start by with Lobby as a container with a room...
-- then add permissions, then maybe options, then games

newtype LobbyId = LobbyId { _unLobbyId :: Int } deriving (Ord, Eq, Data, Typeable, Read, Show)--, SafeCopy)

data Lobby = Lobby
    { _users        :: [UserId]
    , _room         :: RoomId
    }

data LobbyState = LobbyState
    { lobbies       :: IxSet Lobby
    , nextLobbyId   :: LobbyId
    }
