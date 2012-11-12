{-# LANGUAGE DeriveDataTypeable, FlexibleContexts, GeneralizedNewtypeDeriving
  , MultiParamTypeClasses, OverloadedStrings, ScopedTypeVariables
  , TemplateHaskell, TypeFamilies, FlexibleInstances, RecordWildCards
  , TypeOperators #-}

module Server.Auth.Api where

import Control.Monad ( mzero )
import Happstack.Server.RqData
import Happstack.Server             ( Response, ServerPart, ServerPartT, ok
                                    , toResponse, simpleHTTP, nullConf
                                    , seeOther, dir, notFound, seeOther
                                    , Method(..), methodM, FilterMonad
                                    , WebMonad, ServerMonad, Happstack
                                    , mapServerPartT )
import Happstack.Server            (Input, internalServerError, toResponse, unauthorized)


import qualified Data.ByteString.Lazy.Char8 as L
import Happstack.Server
import Happstack.Server.Types
import Control.Monad.IO.Class (liftIO)

import Data.Maybe ( fromJust )

import Data.Data (Data, Typeable)



import Data.Aeson
import Data.Text

data AuthAPI
    = AuthAPILogin Text Text
    | AuthAPILogout
    | AuthAPIRegister Text Text

instance FromJSON AuthAPI where
    parseJSON (Object o) =
        do
            (rqType :: Text) <- o .: "type"
            case rqType of
                "login"     -> do
                                name <- o .: "name"
                                password <- o .: "password"
                                return $ AuthAPILogin name password
                "logout"    -> return $ AuthAPILogout
                "register"  -> do
                                name <- o .: "name"
                                password <- o .: "password"
                                return $ AuthAPIRegister name password
    parseJSON _ = mzero

runAuthRequest :: AuthAPI -> Text
runAuthRequest authAPI =
    case authAPI of
        AuthAPILogin name password     -> "asdf"
        AuthAPILogout                  -> "asdf"
        AuthAPIRegister name password  -> "asdf" 
