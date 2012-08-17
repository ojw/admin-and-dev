{-# LANGUAGE DeriveDataTypeable, FlexibleContexts, GeneralizedNewtypeDeriving
  , MultiParamTypeClasses, OverloadedStrings, ScopedTypeVariables
  , TemplateHaskell, TypeFamilies, FlexibleInstances, RecordWildCards
  , TypeOperators #-}

module Main 

where

import Control.Applicative          ( (<$>), (<*>), Applicative, Alternative )
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
import Prelude  hiding              ( null, (.) )
import qualified System.FilePath as FP
import Text.Boomerang.TH            ( derivePrinterParsers )
import Web.Routes                   ( RouteT, runRouteT, Site(..), setDefault
                                    , MonadRoute, askRouteFn, URL, PathInfo )
import Web.Routes.Boomerang         ( (<>), lit, (</>), anyText, (:-), Router
                                    , xmaph, int, boomerangSite, integer )
import Web.Routes.Happstack         ( implSite )
import Web.Routes.TH                ( derivePathInfo )

import Text.Blaze.Html5 hiding      ( base )
import Text.Blaze.Html5.Attributes hiding ( dir )
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A

import HasAcidState
import Authentication

data Sitemap
    = Home
    | Profile UserId
    | Echo Text
      deriving (Eq, Ord, Read, Show, Data, Typeable)

$(derivePrinterParsers ''Sitemap)

sitemap :: Router () (Sitemap :- ())
sitemap =
    (  rHome
    <> rProfile . (lit "profile" </> userId)
    <> rEcho . (lit "message" </> anyText)
    )
    where userId :: Router () (UserId :- ())
          userId = 
              xmaph UserId (Just . _unUserId) integer

template :: String -> [H.Html] -> H.Html -> H.Html
template title headers body =
    H.docTypeHtml $ do
      H.head $ do
        H.title (H.toHtml title)
      H.body $ do
        H.h1 $ H.toHtml ("Admin and Dev" :: String)
        H.p  $ H.toHtml ("A division of Jolly Crouton Media?  Maybe?  Idk." :: String)
        body

route :: Sitemap -> RouteT Sitemap App Response -- (ServerPartT IO) Response
route url =
    case url of
      Home              -> ok $ toResponse $ template "Title" [] loginBox
      (Profile userId)  -> ok $ toResponse $ template "Profile" [] $ H.toHtml $ "Profile: " ++ show (_unUserId userId)
      (Echo message)    -> ok $ toResponse $ template "Message" [] $ H.toHtml $ "Message: " ++ unpack message

site :: Site Sitemap (App Response) -- (ServerPartT IO Response)
site =
       setDefault Home $ boomerangSite (runRouteT route) sitemap

----------------------------------------------------------------------

withAcid :: Maybe FilePath -> (Acid -> IO a) -> IO a
withAcid mBasePath action =
    let basePath = fromMaybe "_state" mBasePath
    in withLocalState (Just $ basePath FP.</> "auth") initialAuthenticationState $ \c ->
           action (Acid c) 

runApp :: Acid -> App a -> ServerPartT IO a
runApp acid (App sp) = mapServerPartT (flip runReaderT acid) sp

data Acid = Acid { acidAuthState    :: AcidState AuthenticationState }

newtype App a = App { unApp :: ServerPartT (ReaderT Acid IO) a }
    deriving ( Functor, Alternative, Applicative, Monad, MonadPlus, MonadIO
               , HasRqData, ServerMonad ,WebMonad Response, FilterMonad Response
               , Happstack, MonadReader Acid)

instance HasAcidState App AuthenticationState where
    getAcidState = acidAuthState    <$> ask 

main :: IO ()
main = withAcid Nothing $ \acid -> simpleHTTP nullConf $ runApp acid serverPart


serverPart:: App Response -- ServerPartT IO Response
serverPart = msum [ dir "favicon.ico" $ notFound (toResponse ())
                  , implSite "http://localhost:8000" "/route" site
                  , implSite "http://localhose:8000" "/auth" (authSite template)
                  , seeOther ("/route/" :: String) (toResponse ())
                  ]
