{-# LANGUAGE DeriveDataTypeable, FlexibleContexts, GeneralizedNewtypeDeriving
  , MultiParamTypeClasses, OverloadedStrings, ScopedTypeVariables
  , TemplateHaskell, TypeFamilies, FlexibleInstances, RecordWildCards
  , TypeOperators #-}

module Main 

where

import Control.Applicative          ( (<$>), (<*>), Applicative, Alternative )
import Control.Applicative          ( (<*), (*>), optional)
import Control.Category             ( (.) )
import Control.Monad                ( msum, liftM, MonadPlus )
import Control.Monad.Reader         ( ask, ReaderT(..), MonadReader )
import Control.Monad.State          ( get, put, gets )
import Control.Monad.Trans          ( lift, MonadIO(..) )
import Crypto.BCrypt                ( validatePassword, hashPasswordUsingPolicy
                                    , slowerBcryptHashingPolicy )
import Data.Maybe                   ( fromMaybe, fromJust )
import Data.SafeCopy                ( SafeCopy, base, deriveSafeCopy )
import Data.Lens.Template           ( makeLens )
import Data.Acid                    ( AcidState(..), EventState(..)
                                    , EventResult(..) , Query(..)
                                    , QueryEvent(..), Update(..)
                                    , UpdateEvent(..), IsAcidic(..), makeAcidic
                                    , openLocalState)
import Data.Acid.Advanced           ( query', update' )
import Data.Acid.Local              ( createCheckpointAndClose 
                                    , openLocalStateFrom)
import Data.ByteString              ( ByteString, pack )
import Data.Data                    ( Data, Typeable )
import Data.Functor                 ( (<$>) )
import Data.IxSet                   ( Indexable(..), IxSet(..), (@=), Proxy(..)
                                    , getOne, ixFun, updateIx, size, null
                                    , ixSet )
import Data.Lens                    ( (%=), (!=), (^$), (^=), Lens )
import Data.Lens.IxSet              ( ixLens )
import Data.Map                     ( Map, insert, adjust, lookup )
import Data.Text                    ( Text, unpack, reverse, toUpper )
import qualified Data.Text as Text
import Data.Text.Encoding           ( encodeUtf8 )
import Happstack.Server.RqData
import Happstack.Server             ( Response, ServerPart, ServerPartT, ok
                                    , toResponse, simpleHTTP, nullConf
                                    , seeOther, dir, notFound, seeOther
                                    , Method(..), methodM, FilterMonad
                                    , WebMonad, ServerMonad, Happstack
                                    , mapServerPartT )
import Happstack.Server            (Input, internalServerError, toResponse, unauthorized)
import Prelude  hiding              ( null, (.) )
import qualified System.FilePath as FP
import Text.Boomerang.TH            ( derivePrinterParsers )
import Web.Routes                   ( RouteT, runRouteT, Site(..), setDefault
                                    , MonadRoute, askRouteFn, URL, PathInfo, showURL )
import Web.Routes.Boomerang         ( (<>), lit, (</>), anyText, (:-), Router
                                    , xmaph, int, boomerangSite, integer )
import Web.Routes.Happstack         ( implSite )
import Web.Routes.TH                ( derivePathInfo )

import Text.Blaze.Html5 hiding      ( base )
import Text.Blaze.Html5.Attributes as A hiding (label)
import qualified Text.Blaze.Html5 as H
import Text.Blaze.Html5           as H hiding (fieldset, ol, li, label, head)
import qualified Text.Blaze.Html5.Attributes as A
import Text.Reform.Blaze.Text     as R
import Text.Reform.Happstack      as R

import Util.HasAcidState


import Sitemap
import Plugins.Auth
import Acid
import Pages

route :: Sitemap -> RouteT Sitemap App Response
route url =
    case url of
        Home   -> homePage
        Login   -> loginPage
        Logout   -> logoutPage
        Create -> createPage

site :: Site Sitemap (App Response)
site = setDefault Home $ boomerangSite (runRouteT route) sitemap

app:: App Response
app =  
    do  decodeBody (defaultBodyPolicy "/tmp/" 0 1000 1000)
        msum [ Happstack.Server.dir "favicon.ico" $ notFound (toResponse ())
             , implSite "http://localhost:8000" "" site
             , seeOther ("" :: String) (toResponse ())
             ]

main :: IO ()
main = withAcid Nothing $ \acid -> simpleHTTP nullConf $ runApp acid app
