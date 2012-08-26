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
import Crypto.BCrypt                ( validatePassword, hashPasswordUsingPolicy
                                    , slowerBcryptHashingPolicy )
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
import Data.ByteString              ( ByteString, pack )
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
import Web.Routes                   ( RouteT, runRouteT, Site(..), setDefault
                                    , MonadRoute, askRouteFn, URL, PathInfo )
import Web.Routes.Boomerang         ( (<>), lit, (</>), anyText, (:-), Router
                                    , xmaph, int, boomerangSite, integer )
import Web.Routes.Happstack         ( implSite )
import Web.Routes.TH                ( derivePathInfo )


import Authentication               ( UserId )


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

joinRoom :: UserId -> RoomId -> Update RoomState (IxSet Room)
joinRoom uid rid =
    do  roomState <- get
        case getOne $ (rooms ^$ roomState) @= rid of
             Nothing    -> return (rooms ^$ roomState)
             Just rm    -> leaveRoom uid >> modRoom rid (addUserToRoom uid)

speak :: UserId -> Text -> Update RoomState (IxSet Room)
speak uid msg =
    do  roomState <- get
        case getOne $ (rooms ^$ roomState) @= uid of
             Nothing    -> return (rooms ^$ roomState)
             Just rm    -> modRoom (roomId ^$ rm) (addChat uid msg)

listen :: UserId -> Query RoomState [Chat]
listen uid =
    do  roomState <- ask
        case getOne $ (rooms ^$ roomState) @= uid of
             Nothing    -> return []
             Just rm    -> return $ chat ^$ rm

$(makeAcidic ''RoomState ['createRoom, 'joinRoom, 'leaveRoom]) 
