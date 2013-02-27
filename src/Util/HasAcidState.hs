{-# LANGUAGE DeriveDataTypeable, FlexibleContexts, GeneralizedNewtypeDeriving
  , MultiParamTypeClasses, OverloadedStrings, ScopedTypeVariables
  , TemplateHaskell, TypeFamilies, FlexibleInstances, RecordWildCards
  , TypeOperators #-}

module Util.HasAcidState 

where

import Control.Exception.Lifted     ( bracket)
import Control.Monad.Trans          ( MonadIO(..) )
import Control.Monad.Trans.Control  ( MonadBaseControl )
import Data.Acid                    ( AcidState(..), EventState(..)
                                    , EventResult(..) , Query(..)
                                    , QueryEvent(..), Update(..)
                                    , UpdateEvent(..), IsAcidic(..), makeAcidic
                                    , openLocalState)
import Data.Acid.Advanced           ( query', update' )
import Data.Acid.Local              ( createCheckpointAndClose 
                                    , openLocalStateFrom)
import Data.Data                    ( Data, Typeable )

class (Functor m, Monad m) => HasAcidState m st where
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
                  Maybe FilePath        -- ^ path to state directory
               -> st                    -- ^ initial state value
               -> (AcidState st -> m a) -- ^ function which uses the `AcidState` handle
               -> m a
withLocalState mPath initialState =
    bracket (liftIO $ (maybe openLocalState openLocalStateFrom mPath) initialState)
            (liftIO . createCheckpointAndClose) 
