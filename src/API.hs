{-# LANGUAGE DeriveDataTypeable, GeneralizedNewtypeDeriving, TemplateHaskell, 
   TypeOperators, OverloadedStrings #-}

module API where

import Control.Monad        ( msum )
import Web.Routes.Happstack ( implSite )

import Plugins.Room         ( roomAPISite )

apiRoute baseURL = msum [ implSite baseURL "/room" roomAPISite ]
