{-# LANGUAGE OverloadedStrings #-}

module Util.GetBody where

import Control.Monad.Trans          ( MonadIO )
import Control.Monad.IO.Class       ( liftIO )
import Happstack.Server             ( ServerMonad, takeRequestBody, askRq, unBody )
import Data.ByteString.Lazy.Char8   ( ByteString )

import Data.Data (Data, Typeable)


getBody :: (ServerMonad m, MonadIO m) => m ByteString
getBody = do
    req  <- askRq 
    body <- liftIO $ takeRequestBody req 
    case body of 
        Just rqbody -> return . unBody $ rqbody 
        Nothing     -> return "" 
