{-# LANGUAGE DeriveDataTypeable, FlexibleContexts, GeneralizedNewtypeDeriving
  , MultiParamTypeClasses, OverloadedStrings, ScopedTypeVariables
  , TemplateHaskell, TypeFamilies, FlexibleInstances, RecordWildCards
  , TypeOperators #-}

module Plugins.Auth.Acid

( UserId(..)
, AuthState
, initialAuthState
, ProfileState
, initialProfileState
, getUserId'
, getUserId
)

where

import Happstack.Auth.Core.Auth
import Happstack.Auth.Core.Profile

import Happstack.Server             ( Happstack )
import Data.Acid                    ( AcidState )
 
import Util.HasAcidState

getUserId' :: (HasAcidState m AuthState, HasAcidState m ProfileState, Monad m, Happstack m) => m (Maybe UserId)
getUserId' =
    do
        authState :: AcidState AuthState <- getAcidState
        profileState :: AcidState ProfileState <- getAcidState
        getUserId authState profileState

