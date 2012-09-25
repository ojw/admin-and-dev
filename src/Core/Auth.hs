{-# LANGUAGE DeriveDataTypeable, FlexibleContexts, GeneralizedNewtypeDeriving
  , MultiParamTypeClasses, OverloadedStrings, ScopedTypeVariables
  , TemplateHaskell, TypeFamilies, FlexibleInstances, RecordWildCards
  , TypeOperators #-}

module Core.Auth 

( loginForm
, newAccountForm
, UserId(..)
, getUserId
, getUserId'
)

where

import Core.Auth.Acid
import Core.Auth.Api
import Core.Auth.Html
