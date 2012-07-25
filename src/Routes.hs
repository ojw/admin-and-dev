{-# LANGUAGE DeriveDataTypeable, GeneralizedNewtypeDeriving, TemplateHaskell, 
  TypeOperators, OverloadedStrings, QuasiQuotes, NoMonomorphismRestriction #-}

module Routes where

import Prelude                 hiding (head, id, (.))
import Control.Category        (Category(id, (.)))
import Control.Monad           (msum, liftM)
import Web.Routes.TH           (derivePathInfo)
import Data.Data               (Data, Typeable)
import Data.Text               (Text, pack, unpack, reverse, toUpper)
import Web.Routes              ( PathInfo(..), RouteT, showURL
                               , runRouteT, Site(..), setDefault, mkSitePI
                               , MonadRoute, URL, askRouteFn)
import Happstack.Server        ( Response, ServerPartT, ok, toResponse, simpleHTTP
                               , nullConf, seeOther, dir, notFound, seeOther, Method(..), methodM)
import Web.Routes.Boomerang
import Text.Boomerang.TH       (derivePrinterParsers)
import Web.Routes.Happstack    (implSite)
import Text.Hamlet
import Text.Lucius

import Views
import Sitemap (Sitemap(..), unUserId, sitemap)

route :: Sitemap -> RouteT Sitemap (ServerPartT IO) Response
route url =
    case url of
      Home              -> do methodM GET
                              hamletTest
      (Profile userId)  -> ok $ toResponse $ "Profile" ++ show (unUserId userId)
      (Echo message)    -> ok $ toResponse $ "Message" ++ unpack message

site :: Site Sitemap (ServerPartT IO Response)
site =
       setDefault Home $ boomerangSite (runRouteT route) sitemap

main :: IO ()
main = simpleHTTP nullConf $
       msum [ dir "favicon.ico" $ notFound (toResponse ())
            , implSite "http://localhost:8000" "/route" site
            , seeOther ("/route/" :: String) (toResponse ())
            ] 
