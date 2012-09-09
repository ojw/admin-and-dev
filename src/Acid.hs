{-# LANGUAGE DeriveDataTypeable, FlexibleContexts, GeneralizedNewtypeDeriving
  , MultiParamTypeClasses, OverloadedStrings, ScopedTypeVariables
  , TemplateHaskell, TypeFamilies, FlexibleInstances, RecordWildCards
  , TypeOperators #-}

module Acid 

where

import Control.Applicative          ( (<$>), (<*>), Applicative, Alternative )
import Control.Monad                ( MonadPlus )
import Control.Monad.Reader         ( ask, ReaderT(..), MonadReader )
import Control.Monad.Trans          ( MonadIO(..) )
import Data.Maybe                   ( fromMaybe )
import Data.SafeCopy                ( SafeCopy, base, deriveSafeCopy )
import Data.Acid                    ( AcidState(..), EventState(..)
                                    , EventResult(..) , Query(..)
                                    , QueryEvent(..), Update(..)
                                    , UpdateEvent(..), IsAcidic(..), makeAcidic
                                    , openLocalState)
import Data.Acid.Advanced           ( query', update' )
import Data.Acid.Local              ( createCheckpointAndClose 
                                    , openLocalStateFrom)
import Data.Data                    ( Data, Typeable )
import Data.IxSet                   ( Indexable(..), IxSet(..), (@=), Proxy(..)
                                    , getOne, ixFun, updateIx, size, null
                                    , ixSet )
import Happstack.Server.RqData
import Happstack.Server             ( Response, ServerPart, ServerPartT, ok
                                    , toResponse, simpleHTTP, nullConf
                                    , seeOther, dir, notFound, seeOther
                                    , Method(..), methodM, FilterMonad
                                    , WebMonad, ServerMonad, Happstack
                                    , mapServerPartT )
import Happstack.Server            (Input, internalServerError, toResponse, unauthorized)
import Prelude  hiding              ( null )
import System.FilePath              ( (</>) )


import Control.Exception           (bracket)

import Happstack.Auth.Core.Auth
import Happstack.Auth.Core.Profile

import HasAcidState

runApp :: Acid -> App a -> ServerPartT IO a
runApp acid (App sp) = mapServerPartT (flip runReaderT acid) sp

data Acid = Acid
     { acidAuth        :: AcidState AuthState
     , acidProfile     :: AcidState ProfileState
--     , acidProfileData :: AcidState ProfileDataState
     }

newtype App a = App { unApp :: ServerPartT (ReaderT Acid IO) a }
    deriving ( Functor, Alternative, Applicative, Monad, MonadPlus, MonadIO
               , HasRqData, ServerMonad ,WebMonad Response, FilterMonad Response
               , Happstack, MonadReader Acid)

-- need instance for each acid type
instance HasAcidState App AuthState where
    getAcidState = acidAuth    <$> ask 

instance HasAcidState App ProfileState where
    getAcidState = acidProfile    <$> ask 

withAcid :: Maybe FilePath -- ^ state directory
         -> (Acid -> IO a) -- ^ action
         -> IO a
withAcid mBasePath f =
    let basePath = fromMaybe ".state" mBasePath in
    bracket (openLocalStateFrom (basePath </> "auth")        initialAuthState)        (createCheckpointAndClose) $ \auth ->
    bracket (openLocalStateFrom (basePath </> "profile")     initialProfileState)     (createCheckpointAndClose) $ \profile ->
--     bracket (openLocalStateFrom (basePath </> "profileData") initialProfileDataState) (createCheckpointAndClose) $ \profileData ->
        f (Acid auth profile) --profileData) 
