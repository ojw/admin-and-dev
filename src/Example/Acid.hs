{-# LANGUAGE DeriveDataTypeable, FlexibleContexts, GeneralizedNewtypeDeriving
  , MultiParamTypeClasses, OverloadedStrings, ScopedTypeVariables
  , TemplateHaskell, TypeFamilies, FlexibleInstances, RecordWildCards
  , TypeOperators, StandaloneDeriving #-}

module Example.Acid 

( Acid(..)
, withAcid
, Games(..)
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
import Server.Auth.Acid
import Server.Location.Acid
import Server.Game.Acid.GameAcid hiding ( LocationState )

data Games = Dummy

deriving instance Typeable Games
deriving instance Eq Games
deriving instance Ord Games
$(deriveSafeCopy 0 'base ''Games)

data Acid = Acid
    { acidAuth          :: AcidState AuthState
    , acidProfile       :: AcidState ProfileState
    , acidLocation      :: AcidState (LocationState Games)
--    , acidGameHolder    :: AcidState GameHolder
    }

withAcid :: Maybe FilePath -- ^ state directory
         -> (Acid -> IO a) -- ^ action
         -> IO a
withAcid mBasePath f =
    let basePath = fromMaybe ".state" mBasePath in
    bracket (openLocalStateFrom (basePath </> "auth")       initialAuthState)       (createCheckpointAndClose) $ \auth ->
    bracket (openLocalStateFrom (basePath </> "profile")    initialProfileState)    (createCheckpointAndClose) $ \profile ->
    bracket (openLocalStateFrom (basePath </> "location")   initialLocationState)   (createCheckpointAndClose) $ \location ->
--    bracket (openLocalStateFrom (basePath </> "gameholder") initialGameHolder)      (createCheckpointAndClose) $ \gameHolder ->
        f (Acid auth profile location {-gameHolder-})
