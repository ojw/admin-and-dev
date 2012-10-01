{-# LANGUAGE DeriveDataTypeable, FlexibleContexts, GeneralizedNewtypeDeriving
  , MultiParamTypeClasses, OverloadedStrings, ScopedTypeVariables
  , TemplateHaskell, TypeFamilies, FlexibleInstances, RecordWildCards
  , TypeOperators, StandaloneDeriving #-}

module Acid 

( Acid(..)
, withAcid
, Game
)

where

import Data.Maybe                   ( fromMaybe )
import Data.Acid                    ( AcidState )
import Data.Acid.Local              ( createCheckpointAndClose 
                                    , openLocalStateFrom)
import Data.SafeCopy
import Data.Data
import System.FilePath              ( (</>) )
import Control.Exception            ( bracket )

import Util.HasAcidState
import Core.Auth.Acid
import Core.Room.Acid            ( RoomState, initialRoomState )
import Core.Location.Location

data Game = Dummy

deriving instance Typeable Game
deriving instance Eq Game
deriving instance Ord Game
$(deriveSafeCopy 0 'base ''Game)

data Acid = Acid
    { acidAuth      :: AcidState AuthState
    , acidProfile   :: AcidState ProfileState
    , acidRoom      :: AcidState RoomState  
    , acidLocation  :: AcidState (LocationState Game)
    }

withAcid :: Maybe FilePath -- ^ state directory
         -> (Acid -> IO a) -- ^ action
         -> IO a
withAcid mBasePath f =
    let basePath = fromMaybe ".state" mBasePath in
    bracket (openLocalStateFrom (basePath </> "auth")       initialAuthState)       (createCheckpointAndClose) $ \auth ->
    bracket (openLocalStateFrom (basePath </> "profile")    initialProfileState)    (createCheckpointAndClose) $ \profile ->
    bracket (openLocalStateFrom (basePath </> "room")       initialRoomState)       (createCheckpointAndClose) $ \room ->
    bracket (openLocalStateFrom (basePath </> "location")   initialLocationState)   (createCheckpointAndClose) $ \location ->
        f (Acid auth profile room location)
