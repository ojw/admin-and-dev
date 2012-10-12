{-# LANGUAGE DeriveDataTypeable, FlexibleContexts, GeneralizedNewtypeDeriving
  , MultiParamTypeClasses, OverloadedStrings, ScopedTypeVariables
  , TemplateHaskell, TypeFamilies, FlexibleInstances, RecordWildCards
  , TypeOperators #-}

module Core.Room.Acid

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

import Core.Auth.Auth               ( UserId )

import Util.HasAcidState


newtype RoomId = RoomId { _unRoomId :: Integer } deriving (Eq, Ord, Enum, Data, Typeable, SafeCopy, Read, Show)

$(makeLens ''RoomId)

newtype Chat = Chat (UserId, Text) deriving (Eq, Ord, Data, Typeable, SafeCopy, Read, Show) 

data Room = Room
    { _roomId :: RoomId
    , _chat :: [Chat]
    } deriving (Eq, Ord, Data, Typeable, Read, Show)

$(makeLens ''Room)
$(deriveSafeCopy 0 'base ''Room)

instance Indexable Room where
    empty = ixSet [ ixFun $ \room -> [ roomId ^$ room ]
                  ]

data RoomState = RoomState
    { _nextRoomId   :: RoomId
    , _rooms        :: IxSet Room
    }
    deriving (Eq, Ord, Typeable)

$(makeLens ''RoomState)
$(deriveSafeCopy 0 'base ''RoomState)

-- includes default lobby room w/ RoomId 0

initialRoomState :: RoomState
initialRoomState = RoomState
    { _nextRoomId   = RoomId 2
    , _rooms        = updateIx (RoomId 1) (Room (RoomId 1) []) empty
    }

room :: (Typeable key) => key -> Lens (IxSet Room) (Maybe Room)
room = ixLens          

addChat :: UserId -> Text -> Room -> Room
addChat uid msg rm = (chat ^%= (Chat (uid, msg) : )) rm

blankRoom :: RoomId -> Room
blankRoom rid = Room rid []

modRoom :: RoomId -> (Room -> Room) -> Update RoomState (IxSet Room)
modRoom rid fn = rooms %= (room rid ^%= fmap fn)

createRoom :: Update RoomState RoomId
createRoom =
    do  roomState <- get
        let next = nextRoomId ^$ roomState
        rooms %= updateIx next (blankRoom next)
        nextRoomId %= succ
        return next

send :: UserId -> RoomId -> Text -> Update RoomState (IxSet Room)
send userId roomId message = modRoom roomId (addChat userId message)

receive :: UserId -> RoomId -> Query RoomState [Chat]
receive userId roomId =
    do  roomState <- ask
        case getOne $ (rooms ^$ roomState) @= roomId of
             Nothing    -> return []
             Just rm    -> return $ chat ^$ rm

lookRooms :: Query RoomState [Room]
lookRooms =
    do  roomState <- ask
        return $ toList $ rooms ^$ roomState

$(makeAcidic ''RoomState ['createRoom, 'send, 'receive, 'lookRooms])
