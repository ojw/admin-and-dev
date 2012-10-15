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
data Lobby game = Lobby
    { _lobbyId  :: LobbyId
    , _roomId   :: RoomId
    , _game     :: game
    } -- deriving (Eq, Ord, Read, Show, Data, Typeable)

deriving instance Eq game => Eq (Lobby game)
deriving instance Ord game => Ord (Lobby game)
deriving instance Read game => Read (Lobby game)
deriving instance Show game => Show (Lobby game)
deriving instance Data game => Data (Lobby game)
deriving instance Typeable1 Lobby

$(makeLens ''Lobby)
$(deriveSafeCopy 0 'base ''Lobby)

instance (Ord game, Typeable game) => Indexable (Lobby game) where
    empty = ixSet [ ixFun $ \lobby -> [ lobbyId ^$ lobby ]
                  , ixFun $ \lobby -> [ roomId ^$ lobby ]
                  , ixFun $ \lobby -> [ game ^$ lobby ]
                  ]

data LobbyState game = LobbyState
    { _nextLobbyId  :: LobbyId
    , _lobbies      :: IxSet (Lobby game)
    }

deriving instance (Ord game, Typeable game, Eq game) => Eq (LobbyState game)
deriving instance (Typeable game, Ord game) => Ord (LobbyState game)
deriving instance (Typeable game, Ord game, Read game) => Read (LobbyState game)
deriving instance (Ord game, Show game) => Show (LobbyState game)
deriving instance (Ord game, Data game) => Data (LobbyState game)
deriving instance Typeable1 LobbyState

initialLobbyState :: (Ord game, Typeable game) => LobbyState game
initialLobbyState = LobbyState (LobbyId 1) empty

$(makeLens ''LobbyState)

instance (Ord game, Typeable game, SafeCopy game) => SafeCopy (LobbyState game) where
    putCopy (LobbyState nextLobbyId lobbies) = contain $ do safePut nextLobbyId; safePut lobbies;
    getCopy = contain $ LobbyState <$> safeGet <*> safeGet

getRoomId' :: (Ord game, Typeable game) => LobbyId -> LobbyState game -> (Maybe RoomId)
getRoomId' lobbyId lobbyState = fmap _roomId $ getOne $ (lobbies ^$ lobbyState) @= lobbyId

getRoomId :: (Ord game, Typeable game) => LobbyId -> Query (LobbyState game) (Maybe RoomId)
getRoomId lobbyId = 
    do  lobbyState <- ask
        return $ getRoomId' lobbyId lobbyState

$(makeAcidic ''LobbyState ['getRoomId]) 
