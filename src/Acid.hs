{-# LANGUAGE DeriveDataTypeable, FlexibleContexts, GeneralizedNewtypeDeriving
  , MultiParamTypeClasses, OverloadedStrings, ScopedTypeVariables
  , TemplateHaskell, TypeFamilies, FlexibleInstances, RecordWildCards
  , TypeOperators #-}

module Acid 

where

import Data.Maybe                   ( fromMaybe )
import Data.Acid                    ( AcidState )
import Data.Acid.Local              ( createCheckpointAndClose 
                                    , openLocalStateFrom)
import System.FilePath              ( (</>) )


import Control.Exception            ( bracket )

import Util.HasAcidState
import Happstack.Auth.Core.Auth
import Happstack.Auth.Core.Profile
import Plugins.Room                 ( RoomState, initialRoomState )

data Acid = Acid
    { acidAuth      :: AcidState AuthState
    , acidProfile   :: AcidState ProfileState
    , acidRoom      :: AcidState RoomState  
    }

withAcid :: Maybe FilePath -- ^ state directory
         -> (Acid -> IO a) -- ^ action
         -> IO a
withAcid mBasePath f =
    let basePath = fromMaybe ".state" mBasePath in
    bracket (openLocalStateFrom (basePath </> "auth")       initialAuthState)       (createCheckpointAndClose) $ \auth ->
    bracket (openLocalStateFrom (basePath </> "profile")    initialProfileState)    (createCheckpointAndClose) $ \profile ->
    bracket (openLocalStateFrom (basePath </> "room")       initialRoomState)       (createCheckpointAndClose) $ \room ->
        f (Acid auth profile room)
