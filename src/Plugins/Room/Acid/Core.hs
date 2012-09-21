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
    , _members :: [UserId]
    , _chat :: [Chat]
    } deriving (Eq, Ord, Data, Typeable, Read, Show)

$(makeLens ''Room)
$(deriveSafeCopy 0 'base ''Room)

instance Indexable Room where
    empty = ixSet [ ixFun $ \room -> [ roomId ^$ room ]
                  , ixFun $ \room -> members ^$ room
                  ]

data RoomState = RoomState
    { _nextRoomId   :: RoomId
    , _rooms        :: IxSet Room
    , _defaultRoom  :: RoomId
    }
    deriving (Eq, Ord, Typeable)

$(makeLens ''RoomState)
$(deriveSafeCopy 0 'base ''RoomState)

-- includes default lobby room w/ RoomId 0

initialRoomState :: RoomState
initialRoomState = RoomState
    { _nextRoomId   = RoomId 1
    , _rooms        = updateIx lobbyId lobby empty
    , _defaultRoom  = lobbyId
    }

lobbyId :: RoomId
lobbyId = RoomId 0

lobby :: Room
lobby = Room lobbyId [] []

--

room :: (Typeable key) => key -> Lens (IxSet Room) (Maybe Room)
room = ixLens          

removeUserFromRoom :: UserId -> Room -> Room
removeUserFromRoom uid rm = (members ^%= (filter (/= uid))) rm

addUserToRoom :: UserId -> Room -> Room
addUserToRoom uid rm = (members ^%= (uid:)) rm

addChat :: UserId -> Text -> Room -> Room
addChat uid msg rm = (chat ^%= (Chat (uid, msg) : )) rm

blankRoom :: RoomId -> Room
blankRoom rid = Room rid [] []

modRoom :: RoomId -> (Room -> Room) -> Update RoomState (IxSet Room)
modRoom rid fn = rooms %= (room rid ^%= fmap fn)

createEmptyRoom :: Update RoomState RoomId
createEmptyRoom =
    do  roomState <- get
        let next = nextRoomId ^$ roomState
        rooms %= updateIx next (blankRoom next)
        nextRoomId %= succ
        return next

-- this doesn't even remove the user from their other rooms
-- clearly the room / user relationship must be rethought
createRoom :: UserId -> Update RoomState RoomId
createRoom uid = 
    do  roomState <- get
        let next = nextRoomId ^$ roomState
        rooms %= updateIx next (Room next [uid] [])
        nextRoomId %= succ
        return next

getUserRoomsIx :: UserId -> IxSet Room -> [Room]
getUserRoomsIx uid rms = toList $ rms @= uid

-- ********** EXTREMELY DISHONEST *****************
--getUserRoom :: UserId -> Query RoomState RoomId
getUserRoom uid = return (RoomId 1)
-- ********** TEMPORARY FOR TROUBLESHOOTING *******

-- rethink join / leave situation
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
    do  userRoom    <- getUserRoom uid
        modRoom userRoom (addChat uid msg)

receive :: UserId -> Query RoomState [Chat]
receive uid =
    do  roomState <- ask
        userRoom    <- getUserRoom uid
        case getOne $ (rooms ^$ roomState) @= userRoom of
             Nothing    -> return []
             Just rm    -> return $ chat ^$ rm

lookRooms :: Query RoomState [Room]
lookRooms =
    do  roomState <- ask
        return $ toList $ rooms ^$ roomState

$(makeAcidic ''RoomState ['createRoom, 'joinRoom, 'leaveRoom, 'send, 'receive, 'lookRooms])
