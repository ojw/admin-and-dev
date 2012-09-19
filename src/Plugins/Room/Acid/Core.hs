{-# LANGUAGE DeriveDataTypeable, FlexibleContexts, GeneralizedNewtypeDeriving
  , MultiParamTypeClasses, OverloadedStrings, ScopedTypeVariables
  , TemplateHaskell, TypeFamilies, FlexibleInstances, RecordWildCards
  , TypeOperators #-}

module Plugins.Room.Acid.Core

where

import Control.Category             ( (.) )
import Control.Monad.Reader         ( ask )
import Control.Monad.State          ( get, put )
import Data.SafeCopy                ( SafeCopy, base, deriveSafeCopy )
import Data.Lens.Template           ( makeLens )
import Data.Acid                    ( AcidState(..), EventState(..)
                                    , EventResult(..) , Query(..)
                                    , QueryEvent(..), Update(..)
                                    , UpdateEvent(..), IsAcidic(..), makeAcidic
                                    , openLocalState)
import Data.Data                    ( Data, Typeable )
import Data.IxSet                   ( Indexable(..), IxSet(..), (@=), Proxy(..)
                                    , getOne, ixFun, updateIx, size, null
                                    , ixSet, toList )
import Data.Lens                    ( (%=), (!=), (^$), (^=), Lens, (^%=) )
import Data.Lens.IxSet              ( ixLens )
import Data.Text                    ( Text )
import Prelude  hiding              ( null, (.) )

import Plugins.Auth                 ( UserId )

import Util.HasAcidState


newtype RoomId = RoomId { _unRoomId :: Integer } deriving (Eq, Ord, Enum, Data, Typeable, SafeCopy, Read, Show)

$(makeLens ''RoomId)

newtype Chat = Chat (UserId, Text) deriving (Eq, Ord, Data, Typeable, SafeCopy, Read, Show) 

data Room = Room
    { _roomId :: RoomId
    , _capacity :: Int
    , _members :: [UserId]
    , _chat :: [Chat]
    } deriving (Eq, Ord, Data, Typeable, Read, Show)

$(makeLens ''Room)
$(deriveSafeCopy 0 'base ''Room)

instance Indexable Room where
    empty = ixSet [ ixFun $ \room -> [ roomId ^$ room ]
                  , ixFun $ \room -> [ capacity ^$ room ]
                  , ixFun $ \room -> members ^$ room
                  ]

data RoomState = RoomState
    { _nextRoomId   :: RoomId
    , _rooms        :: IxSet Room
    }
    deriving (Eq, Ord, Typeable)

$(makeLens ''RoomState)
$(deriveSafeCopy 0 'base ''RoomState)

initialRoomState :: RoomState
initialRoomState = RoomState
    { _nextRoomId = RoomId 1
    , _rooms      = empty
    }

room :: (Typeable key) => key -> Lens (IxSet Room) (Maybe Room)
room = ixLens          

removeUserFromRoom :: UserId -> Room -> Room
removeUserFromRoom uid rm = (members ^%= (filter (/= uid))) rm

addUserToRoom :: UserId -> Room -> Room
addUserToRoom uid rm = (members ^%= (uid:)) rm

modRoom :: RoomId -> (Room -> Room) -> Update RoomState (IxSet Room)
modRoom rid fn = rooms %= (room rid ^%= fmap fn)

addChat :: UserId -> Text -> Room -> Room
addChat uid msg rm = (chat ^%= (Chat (uid, msg) : )) rm

createRoom :: UserId -> Int -> Update RoomState RoomId
createRoom uid cap = 
    do  roomState <- get
        let next = nextRoomId ^$ roomState
        rooms %= updateIx next (Room next cap [uid] [])
        nextRoomId %= succ
        return next

getUserRoomsIx :: UserId -> IxSet Room -> [Room]
getUserRoomsIx uid rms = toList $ rms @= uid

leaveRoom :: UserId -> Update RoomState (Maybe Room)
leaveRoom uid = (room uid) . rooms %= fmap (removeUserFromRoom uid)

-- if single user ever joins multiple rooms it will be a problem
-- this should be impossible, but one never knows
joinRoom :: UserId -> RoomId -> Update RoomState (IxSet Room)
joinRoom uid rid =
    do  roomState <- get
        case getOne $ (rooms ^$ roomState) @= rid of
             Nothing    -> modRoom rid (addUserToRoom uid) -- return (rooms ^$ roomState)
             Just rm    -> leaveRoom uid >> modRoom rid (addUserToRoom uid)

send :: UserId -> Text -> Update RoomState (IxSet Room)
send uid msg =
    do  roomState <- get
        case getOne $ (rooms ^$ roomState) @= uid of
             Nothing    -> return (rooms ^$ roomState)
             Just rm    -> modRoom (roomId ^$ rm) (addChat uid msg)

receive :: UserId -> Query RoomState [Chat]
receive uid =
    do  roomState <- ask
        case getOne $ (rooms ^$ roomState) @= uid of
             Nothing    -> return []
             Just rm    -> return $ chat ^$ rm

lookRooms :: Query RoomState [Room]
lookRooms =
    do  roomState <- ask
        return $ toList $ rooms ^$ roomState

$(makeAcidic ''RoomState ['createRoom, 'joinRoom, 'leaveRoom, 'send, 'receive, 'lookRooms])
