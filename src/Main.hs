{-# LANGUAGE MultiParamTypeClasses, GeneralizedNewtypeDeriving #-}

module Main where

import Control.Monad.Reader (MonadReader, ReaderT(..), ask)
import Control.Applicative  ( Applicative, Alternative, (<$>) )
import Control.Monad        ( MonadPlus )
import Control.Monad.Trans  (MonadIO(..))
import Happstack.Server     ( Happstack, HasRqData, Method(GET, POST), Request(rqMethod)
                            , Response
                            , ServerPartT(..), WebMonad, FilterMonad, ServerMonad
                            , askRq, decodeBody, dir, defaultBodyPolicy, lookText
                            , mapServerPartT, nullConf, nullDir, ok, simpleHTTP
                            , toResponse
                            )

import Database

newtype App a = App { unApp :: ServerPartT (ReaderT Acid IO) a }
    deriving ( Functor, Alternative, Applicative, Monad, MonadPlus, MonadIO
               , HasRqData, ServerMonad ,WebMonad Response, FilterMonad Response
               , Happstack, MonadReader Acid)

runApp :: Acid -> App a -> ServerPartT IO a
runApp acid (App sp) = mapServerPartT (flip runReaderT acid) sp

instance HasAcidState App UserState where
    getAcidState = acidUserState <$> ask 

instance HasAcidState App RoomState where
    getAcidState = acidRoomState <$> ask
