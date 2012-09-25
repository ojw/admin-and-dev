{-# LANGUAGE DeriveDataTypeable, GeneralizedNewtypeDeriving, TemplateHaskell, 
   TypeOperators, OverloadedStrings #-}

module Api

( apiSite
)

where

import Control.Monad        ( msum )
import Web.Routes.Happstack ( implSite )
import Data.Text            ( append )

import Core.Room         ( roomAPISite )

apiSite baseURL dir = msum [ implSite baseURL (append dir  "/room") roomAPISite ]
