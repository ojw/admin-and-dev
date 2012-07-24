{-# LANGUAGE DeriveDataTypeable, FlexibleContexts, GeneralizedNewtypeDeriving
  , MultiParamTypeClasses, OverloadedStrings, ScopedTypeVariables, TemplateHaskell
  , TypeFamilies, FlexibleInstances, RecordWildCards #-}

module Main where

import Control.Monad.State (get, put)
import Control.Monad.Reader (ask)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as Text
import Data.SafeCopy        (SafeCopy, base, deriveSafeCopy)
import Data.Data            (Data, Typeable)
import Data.Lens            ((%=), (!=), (^$))
import Data.Lens.Template   (makeLens)
import Data.Acid            ( AcidState(..), EventState(..), EventResult(..)
                            , Query(..), QueryEvent(..), Update(..), UpdateEvent(..)
                            , IsAcidic(..), makeAcidic, openLocalState
                            )
import Data.Acid.Local      ( createCheckpointAndClose
                            , openLocalStateFrom
                            )
import Data.Acid.Advanced   (query', update')
import Data.IxSet           ( Indexable(..), IxSet(..), (@=), Proxy(..), getOne
                            , ixFun, ixSet, updateIx, size )
import Data.Lens.IxSet      (ixLens)
import qualified Data.IxSet as IxSet


import Control.Applicative  (Applicative, Alternative, (<$>))
import Control.Exception.Lifted    (bracket)
import Control.Monad.Trans.Control (MonadBaseControl)
import Control.Monad        (MonadPlus, mplus)
import Control.Monad.Reader (MonadReader, ReaderT(..), ask)
import Control.Monad.Trans  (MonadIO(..))

import Happstack.Server     ( Happstack, HasRqData, Method(GET, POST), Request(rqMethod)
                            , Response
                            , ServerPartT(..), WebMonad, FilterMonad, ServerMonad
                            , askRq, decodeBody, dir, defaultBodyPolicy, lookText
                            , mapServerPartT, nullConf, nullDir, ok, simpleHTTP
                            , toResponse
                            )
import System.FilePath      ((</>))

newtype UserId = UserId { _unUserId :: Integer }
    deriving (Eq, Ord, Data, Enum, Typeable, SafeCopy)

$(makeLens ''UserId)

data User = User
    { _userId   :: UserId
    , _email    :: Text
    , _name     :: Text
    , _password :: Text
    }
    deriving (Ord, Eq, Data, Typeable)

$(deriveSafeCopy 0 'base ''User)
$(makeLens ''User)

newtype Email = Email Text deriving (Eq, Ord, Data, Typeable, SafeCopy)
newtype Name = Name Text deriving (Eq, Ord, Data, Typeable, SafeCopy)
newtype Password = Password Text deriving (Eq, Ord, Data, Typeable, SafeCopy)

instance Indexable User where
    empty = ixSet [ ixFun $ \user -> [ userId  ^$ user ]
                  , ixFun $ \user -> [ Email  $ email ^$ user  ]
                  , ixFun $ \user -> [ Name $ name ^$ user ]
                  ]

setEmail :: Text -> Update User Text
setEmail newEmail = email != newEmail

$(makeAcidic ''User ['setEmail])

data UserState = UserState
    { _nextUserId   :: UserId
    , _users        :: IxSet User
    }
    deriving (Eq, Ord, Typeable)

$(makeLens ''UserState)
$(deriveSafeCopy 0 'base ''UserState)

initialUserState :: UserState
initialUserState = UserState
    { _nextUserId = UserId 1
    , _users      = empty
    }

countUsers :: Query UserState Int
countUsers =  do us <- ask
                 return $ size $ users ^$ us

addUser :: Text -> Text -> Text -> Update UserState UserId
addUser email name password = do u@UserState{..} <- get

                                 users != updateIx (nextUserId ^$ u) ( User (nextUserId ^$ u) email name password ) (users ^$ u)
                                 nextUserId %= succ


$(makeAcidic ''UserState ['addUser]) 


-- the HasAcidState trick

class HasAcidState m st where
   getAcidState :: m (AcidState st)
query :: forall event m. 
         ( Functor m
         , MonadIO m
         , QueryEvent event
         , HasAcidState m (EventState event)
         ) => 
         event
      -> m (EventResult event)
query event =
    do as <- getAcidState
       query' (as :: AcidState (EventState event)) event
update :: forall event m. 
          ( Functor m
          , MonadIO m
          , UpdateEvent event
          , HasAcidState m (EventState event)
          ) => 
          event 
       -> m (EventResult event)
update event =
    do as <- getAcidState
       update' (as :: AcidState (EventState event)) event

-- | bracket the opening and close of the `AcidState` handle. 
-- automatically creates a checkpoint on close
withLocalState :: (MonadBaseControl IO m, MonadIO m, IsAcidic st, Typeable st) => 
                  Maybe FilePath           -- ^ path to state directory
                 -> st                     -- ^ initial state value
                 -> (AcidState st -> m a) -- ^ function which uses the `AcidState` handle
                 -> m a
withLocalState mPath initialState =
    bracket (liftIO $ (maybe openLocalState openLocalStateFrom mPath) initialState)
            (liftIO . createCheckpointAndClose)

















data Acid = Acid { acidUserState    :: AcidState UserState
                 }

withAcid :: Maybe FilePath -> (Acid -> IO a) -> IO a
withAcid mBasePath action =
    let basePath = fromMaybe "_state" mBasePath
    in withLocalState (Just $ basePath </> "user")    initialUserState    $ \c ->
           action (Acid c)

newtype App a = App { unApp :: ServerPartT (ReaderT Acid IO) a }
    deriving ( Functor, Alternative, Applicative, Monad, MonadPlus, MonadIO
               , HasRqData, ServerMonad ,WebMonad Response, FilterMonad Response
               , Happstack, MonadReader Acid)

runApp :: Acid -> App a -> ServerPartT IO a
runApp acid (App sp) = mapServerPartT (flip runReaderT acid) sp

instance HasAcidState App UserState where
    getAcidState = acidUserState    <$> ask 
