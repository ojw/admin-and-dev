{-# LANGUAGE DeriveDataTypeable, FlexibleContexts, GeneralizedNewtypeDeriving
  , MultiParamTypeClasses, OverloadedStrings, ScopedTypeVariables
  , TemplateHaskell, TypeFamilies, FlexibleInstances, RecordWildCards
  , TypeOperators #-}

module Plugins.Auth 

( loginForm
, newAccountForm
, UserId(..)
, getUserId
, getUserId'
)

where

import Plugins.Auth.Acid
import Plugins.Auth.API
import Plugins.Auth.HTML
