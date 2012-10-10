{-# LANGUAGE DeriveDataTypeable, GADTs, TemplateHaskell, GeneralizedNewtypeDeriving,
    OverloadedStrings, StandaloneDeriving, TypeFamilies, ScopedTypeVariables #-}

module Core.Lobby.Acid

where

import Control.Applicative hiding (empty)
import Control.Monad.Reader 
import Data.IxSet
import Data.Acid
import Data.SafeCopy
import Data.Data
import Data.Lens
import Data.Lens.Template
import Data.Aeson
import Data.Text hiding (empty)
import Data.ByteString.Lazy as L hiding (empty)
import Happstack.Server

import Core.Auth.Acid        ( UserId )
import Core.Room.Acid        ( RoomId )
import Core.Room.Api
import Util.HasAcidState
import Util.GetBody

newtype LobbyId = LobbyId { _unLobbyId :: Int } deriving (Eq, Ord, Read, Show, Data, Typeable, SafeCopy)

-- will have permission type later
data Lobby = Lobby
    { _lobbyId      :: LobbyId
    , _roomId       :: RoomId
    } deriving (Eq, Ord, Read, Show, Data, Typeable)

--initialLobby :: RoomId -> Lobby
--initialLobby roomId = Lobby roomId

$(makeLens ''Lobby)
$(deriveSafeCopy 0 'base ''Lobby)

instance Indexable Lobby where
    empty = ixSet [ ixFun $ \lobby -> [ roomId ^$ lobby ]
                  ]

data LobbyState = LobbyState
    { _nextLobbyId  :: LobbyId
    , _lobbies      :: IxSet Lobby
    } deriving (Eq, Ord, Read, Show, Data, Typeable)

initialLobbyState = LobbyState (LobbyId 1) empty

$(makeLens ''LobbyState)
$(deriveSafeCopy 0 'base ''LobbyState)

getRoomId' :: LobbyId -> LobbyState -> (Maybe RoomId)
getRoomId' lobbyId lobbyState = fmap _roomId $ getOne $ (lobbies ^$ lobbyState) @= lobbyId

getRoomId :: LobbyId -> Query LobbyState (Maybe RoomId)
getRoomId lobbyId = 
    do  lobbyState <- ask
        return $ getRoomId' lobbyId lobbyState

$(makeAcidic ''LobbyState ['getRoomId])
