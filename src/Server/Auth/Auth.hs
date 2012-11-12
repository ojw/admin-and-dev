{-# LANGUAGE DeriveDataTypeable, FlexibleContexts, GeneralizedNewtypeDeriving
  , MultiParamTypeClasses, OverloadedStrings, ScopedTypeVariables
  , TemplateHaskell, TypeFamilies, FlexibleInstances, RecordWildCards
  , TypeOperators #-}

module Server.Auth.Auth

( loginForm
, newAccountForm
, UserId(..)
, getUserId
, getUserId'
)

where

import Server.Auth.Acid
import Server.Auth.Api
import Server.Auth.Html
