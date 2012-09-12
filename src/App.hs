{-# LANGUAGE DeriveDataTypeable, FlexibleContexts, GeneralizedNewtypeDeriving
  , MultiParamTypeClasses, OverloadedStrings, ScopedTypeVariables
  , TemplateHaskell, TypeFamilies, FlexibleInstances, RecordWildCards
  , TypeOperators #-}

module App 

where

import Control.Applicative          ( (<$>), Applicative, Alternative )
import Control.Monad                ( MonadPlus )
import Control.Monad.Reader         ( ask, ReaderT(..), MonadReader )
import Control.Monad.Trans          ( MonadIO(..) )
import Happstack.Server.RqData      ( HasRqData )
import Happstack.Server             ( Response, ServerPartT, FilterMonad
                                    , WebMonad, ServerMonad, Happstack
                                    , mapServerPartT )

import Happstack.Auth.Core.Auth
import Happstack.Auth.Core.Profile
import Plugins.Room                 ( RoomState, initialRoomState )
import Util.HasAcidState
import Acid



newtype App a = App { unApp :: ServerPartT (ReaderT Acid IO) a }
    deriving ( Functor, Alternative, Applicative, Monad, MonadPlus, MonadIO
               , HasRqData, ServerMonad ,WebMonad Response, FilterMonad Response
               , Happstack, MonadReader Acid) 

-- need instance for each acid type
instance HasAcidState App AuthState where
    getAcidState = acidAuth <$> ask 

instance HasAcidState App ProfileState where
    getAcidState = acidProfile <$> ask 

instance HasAcidState App RoomState where
    getAcidState = acidRoom <$> ask

runApp :: Acid -> App a -> ServerPartT IO a
runApp acid (App sp) = mapServerPartT (flip runReaderT acid) sp