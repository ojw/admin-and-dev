{-# LANGUAGE DeriveDataTypeable, FlexibleContexts, GeneralizedNewtypeDeriving
  , MultiParamTypeClasses, OverloadedStrings, ScopedTypeVariables
  , TemplateHaskell, TypeFamilies, FlexibleInstances, RecordWildCards
  , TypeOperators #-}

module Server.Game.Acid.Types.Room

where

import Data.SafeCopy        ( SafeCopy, base, deriveSafeCopy )
import Data.Lens.Template   ( makeLens )
import Data.Data            ( Data, Typeable )
import Data.IxSet
import Data.Lens
import Data.Text            ( Text )

import Server.Profile.Acid    ( UserName )
import Server.Auth.Acid       ( UserId )

newtype RoomId = RoomId { _unRoomId :: Integer } deriving (Eq, Ord, Enum, Data, Typeable, SafeCopy, Read, Show)

$(makeLens ''RoomId)

newtype Chat = Chat (UserName, Text) deriving (Eq, Ord, Data, Typeable, SafeCopy, Read, Show) 

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
    deriving (Eq, Ord, Read, Show, Data, Typeable)

$(makeLens ''RoomState)
$(deriveSafeCopy 0 'base ''RoomState)

initialRoomState :: RoomState
initialRoomState = RoomState
    { _nextRoomId   = RoomId 1
    , _rooms        = empty
    }
