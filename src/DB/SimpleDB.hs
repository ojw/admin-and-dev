{-# LANGUAGE DeriveDataTypeable, FlexibleContexts, GeneralizedNewtypeDeriving
  , MultiParamTypeClasses, OverloadedStrings, ScopedTypeVariables
  , TemplateHaskell, TypeFamilies, FlexibleInstances, RecordWildCards
  , TypeOperators, StandaloneDeriving #-}

module DB.SimpleDB where

import Data.Maybe                   ( fromMaybe )
import Data.Acid                    ( AcidState )
import Data.Acid.Local              ( createCheckpointAndClose 
                                    , openLocalStateFrom)
import Data.Acid.Remote
import Data.SafeCopy
import Data.Data
import System.FilePath              ( (</>) )
import Control.Exception            ( bracket )

import Util.HasAcidState
import DB.Acid
import DB.Config

import Common.Location.Instances.Create
import Common.Auth.Types
import Common.Profile.Types

withAcid :: Maybe FilePath -- ^ state directory
         -> (Acid -> IO a) -- ^ action
         -> IO a
withAcid mBasePath f =
    let basePath = fromMaybe ".state" mBasePath in
    bracket (openLocalStateFrom (basePath </> "auth")       initialAuthState)       (createCheckpointAndClose) $ \auth ->
    bracket (openLocalStateFrom (basePath </> "profile")    initialProfileState)    (createCheckpointAndClose) $ \profile ->
    bracket (openLocalStateFrom (basePath </> "location")   initialLocationState)   (createCheckpointAndClose) $ \location ->
        f (Acid auth profile location )

withRemoteAcid :: Maybe DBConfig
               -> (Acid -> IO a)
               -> IO a
withRemoteAcid mDBConfig f =
    let dbConfig = fromMaybe defaultDBConfig mDBConfig in
    bracket (uncurry openRemoteState (authLocation dbConfig))       (createCheckpointAndClose) $ \auth ->
    bracket (uncurry openRemoteState (profileLocation dbConfig))    (createCheckpointAndClose) $ \profile ->
    bracket (uncurry openRemoteState (locationLocation dbConfig))   (createCheckpointAndClose) $ \location ->
        f (Acid auth profile location)
