{-# LANGUAGE DeriveDataTypeable, FlexibleContexts, GeneralizedNewtypeDeriving
  , MultiParamTypeClasses, OverloadedStrings, ScopedTypeVariables
  , TemplateHaskell, TypeFamilies, FlexibleInstances, RecordWildCards
  , TypeOperators #-}

module API.AuthAPI where

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
    = AuthAPI_Login Text Text
    | AuthAPI_Logout
    | AuthAPI_Register Text Text

instance FromJSON AuthAPI where
    parseJSON (Object o) =
        do
            (rqType :: Text) <- o .: "type"
            case rqType of
                "login"     -> do
                                name <- o .: "name"
                                password <- o .: "password"
                                return $ AuthAPI_Login name password
                "logout"    -> return $ AuthAPI_Logout
                "register"  -> do
                                name <- o .: "name"
                                password <- o .: "password"
                                return $ AuthAPI_Register name password

processAuthRequest :: AuthAPI -> Text
processAuthRequest authAPI =
    case authAPI of
        AuthAPI_Login name password     -> "asdf"
        AuthAPI_Logout                  -> "asdf"
        AuthAPI_Register name password  -> "asdf" 
