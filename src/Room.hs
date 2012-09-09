{-# LANGUAGE DeriveDataTypeable, FlexibleContexts, GeneralizedNewtypeDeriving
  , MultiParamTypeClasses, OverloadedStrings, ScopedTypeVariables
  , TemplateHaskell, TypeFamilies, FlexibleInstances, RecordWildCards
  , TypeOperators #-}

module Room 

where

import Control.Applicative          ( (<$>), (<*>) )
import Control.Category             ( (.) )
import Control.Exception.Lifted     ( bracket)
import Control.Monad                ( msum, liftM )
import Control.Monad.Reader         ( ask, ReaderT )
import Control.Monad.State          ( get, put, gets, MonadState )
import Control.Monad.Trans          ( lift, MonadIO(..) )
import Control.Monad.Trans.Control  ( MonadBaseControl )
import Data.Maybe                   ( fromMaybe, fromJust )
import Data.SafeCopy                ( SafeCopy, base, deriveSafeCopy )
import Data.Lens.Template           ( makeLens )
import Data.Acid                    ( AcidState(..), EventState(..)
                                    , EventResult(..) , Query(..)
                                    , QueryEvent(..), Update(..)
                                    , UpdateEvent(..), IsAcidic(..), makeAcidic
                                    , openLocalState)
import Data.Acid.Advanced           ( query', update' )
import Data.Acid.Local              ( createCheckpointAndClose 
                                    , openLocalStateFrom)
import Data.Data                    ( Data, Typeable )
import Data.Functor                 ( (<$>) )
import Data.IxSet                   ( Indexable(..), IxSet(..), (@=), Proxy(..)
                                    , getOne, ixFun, updateIx, size, null
                                    , ixSet, toList )
import Data.Lens                    ( (%=), (!=), (^$), (^=), Lens, (^%=) )
import Data.Lens.IxSet              ( ixLens )
import Data.Map                     ( Map, insert, adjust, lookup )
import Data.Text                    ( Text, unpack, reverse, toUpper )
import qualified Data.Text as Text
import Data.Text.Encoding           ( encodeUtf8 )
import Happstack.Server.RqData
import Happstack.Server             ( Response, ServerPart, ServerPartT, ok
                                    , toResponse, simpleHTTP, nullConf
                                    , seeOther, dir, notFound, seeOther
                                    , Method(..), methodM)
import Prelude  hiding              ( null, (.) )
import Text.Boomerang.TH            ( derivePrinterParsers )

import Data.Aeson

import Auth                         ( UserId )
import HasAcidState


newtype RoomId = RoomId { _unRoomId :: Integer } deriving (Eq, Ord, Enum, Data, Typeable, SafeCopy, Read, Show)

$(makeLens ''RoomId)

type Chat = (UserId, Text)

data Room = Room
    { _roomId :: RoomId
    , _capacity :: Int
    , _members :: [UserId]
    , _chat :: [Chat]
    } deriving (Eq, Ord, Data, Typeable)

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
addChat uid msg rm = (chat ^%= ((uid, msg):)) rm

createRoom :: UserId -> Int -> Update RoomState RoomId
createRoom uid cap = 
    do  roomState <- get
        let next = nextRoomId ^$ roomState
        rooms %= updateIx next (Room next cap [uid] [])
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

$(makeAcidic ''RoomState ['createRoom, 'joinRoom, 'leaveRoom, 'send, 'receive])

data RoomRequest
    = R_Create Int
    | R_Join RoomId
    | R_Leave
    | R_Send Text
    | R_Receive
    deriving (Ord, Eq, Data, Typeable, Read, Show) -- all necessary?

instance FromJSON RoomRequest where
    parseJSON (Object o) =
        do
            (rqType :: Text) <- o .: "type"
            case rqType of
                "create"    -> o .: "capacity" >>= \cap -> return $ R_Create (read cap :: Int)
                "join"      -> o .: "roomId" >>= \rid -> return $ R_Join (RoomId rid)
                "leave"     -> return R_Leave
                "send"      -> o .: "message" >>= \msg -> return $ R_Send msg
                "receive"   -> return R_Receive

processRoomRequest :: (HasAcidState m RoomState, MonadIO m) => UserId -> RoomRequest -> m Text
processRoomRequest uid request =
    do
        (roomState :: AcidState RoomState) <- getAcidState
        case request of
            R_Create cap    -> do (RoomId rid) <- update' roomState (CreateRoom uid cap)
                                  return $ Text.pack $ show rid
            R_Join rid      -> do update' roomState (JoinRoom uid rid)
                                  return "Success" -- this is the wrong behavior
            R_Leave         -> do update' roomState (LeaveRoom uid)
                                  return "Success" -- wrong again
            R_Send msg      -> do update' roomState (Send uid msg)
                                  return "Success" -- so dumb
            R_Receive       -> do chat <- query'  roomState (Receive uid)
                                  return "bla" -- not even trying
