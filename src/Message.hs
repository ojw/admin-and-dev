{-# LANGUAGE DeriveDataTypeable, FlexibleContexts, GeneralizedNewtypeDeriving
  , MultiParamTypeClasses, OverloadedStrings, ScopedTypeVariables, TemplateHaskell
  , TypeFamilies, FlexibleInstances, RecordWildCards #-}

module Message where

import System.FilePath      ((</>))
import Control.Monad.State (get, put)
import Control.Monad.Reader (ask)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as Text
import Data.SafeCopy        ( SafeCopy, base, deriveSafeCopy )
import Data.Data            ( Data, Typeable )
import Data.Lens            ( (%=), (!=), (^$) )
import Data.Lens.Template   ( makeLens )
import Data.Acid            ( AcidState(..), EventState(..), EventResult(..)
                            , Query(..), QueryEvent(..), Update(..), UpdateEvent(..)
                            , IsAcidic(..), makeAcidic, openLocalState
                            )
import Data.Acid.Local      ( createCheckpointAndClose
                            , openLocalStateFrom
                            )
import Data.Acid.Advanced   ( query', update' )
import Data.IxSet           ( Indexable(..), IxSet(..), (@=), Proxy(..), getOne
                            , ixFun, ixSet, updateIx, size, toList )
-- import Data.Lens.IxSet      (ixLens)
import Control.Exception.Lifted    (bracket)
import Control.Monad.Trans.Control (MonadBaseControl)
import Control.Monad.Trans  (MonadIO(..))

import Database

newtype MessageId = MessageId { _unMessageId :: Integer }
    deriving (Eq, Ord, Data, Enum, Typeable, SafeCopy, Read, Show)

type Date = Int

data Recipient = RecUser UserId | RecRoom RoomId
    deriving (Eq, Ord, Data, Typeable)

$(deriveSafeCopy 0 'base ''Recipient)

$(makeLens ''MessageId)

data Message = Message
    { _messageId    :: MessageId
    , _sender       :: UserId
    , _recipient    :: Recipient
    , _date         :: Date
    , _body         :: Text
    }
    deriving (Ord, Eq, Data, Typeable)

$(deriveSafeCopy 0 'base ''Message)
$(makeLens ''Message)

instance Indexable Message where
    empty = ixSet [ ixFun $ \message -> [ messageId  ^$ message ]
                  , ixFun $ \message -> [ sender ^$ message  ]
                  , ixFun $ \message -> [ recipient ^$ message ]
                  , ixFun $ \message -> [ date ^$ message ]
                  ]

$(makeAcidic ''Message [])

data MessageState = MessageState
    { _nextMessageId :: MessageId
    , _messages      :: IxSet Message
    }
    deriving (Eq, Ord, Typeable)

$(makeLens ''MessageState)

sendMessage :: Message -> Update MessageState MessageId
sendMessage message = 
    do ms <- get
       messages %= updateIx (nextMessageId ^$ ms) message
       nextMessageId %= succ

receiveMessages :: User -> Query MessageState [Message]
receiveMessages user =
    do messageState <- ask
       case room ^$ user of
        Nothing     -> return []
        (Just r)    -> return $ toList $ (messages ^$ messageState) @= (RecRoom r)

$(deriveSafeCopy 0 'base ''MessageState)

initialMessageState :: MessageState
initialMessageState = MessageState
    { _nextMessageId    = MessageId 1
    , _messages         = empty
    }

$(makeAcidic ''MessageState []) 
