{-# LANGUAGE DeriveDataTypeable, GeneralizedNewtypeDeriving, TemplateHaskell, 
  TypeOperators, OverloadedStrings, QuasiQuotes, NoMonomorphismRestriction #-}

module Routes where

import Control.Monad           ( msum, liftM )
import Data.Text               ( Text, unpack, reverse, toUpper ) -- should be unnecessary soon?
import Web.Routes              ( RouteT, runRouteT, Site(..), setDefault
                               , MonadRoute, askRouteFn, URL )
import Happstack.Server        ( Response, ServerPartT, ok, toResponse, simpleHTTP
                               , nullConf, seeOther, dir, notFound, seeOther, Method(..), methodM)
-- should remove ok and toResponse soonish? or do they belong here and not in template?
-- possibly keep here and templates only give html, not response?
-- makes more sense based on imports
-- dir will probably be temporary
import Web.Routes.Boomerang    ( boomerangSite )
import Web.Routes.Happstack    ( implSite )

import Views (hamletTest)
import Sitemap (Sitemap(..), unUserId, sitemap) -- soon just Sitemap() and sitemap

-- from Tazjin's blog
convRender :: (url -> [(Text, Maybe Text)] -> Text)
           -> (url -> [(Text, Text)]-> Text)
convRender maybeF = 
  (\url params -> maybeF url $ map (\(t1, t2) -> (t1, Just t2)) params)

-- from Tazjin's blog
renderFunction :: MonadRoute m => m (URL m -> [(Text, Text)] -> Text)
renderFunction = liftM convRender $ askRouteFn

-- might just convert from Hamlet to Blaze... if Clayton would be comfortable learning it

route :: Sitemap -> RouteT Sitemap (ServerPartT IO) Response
route url =
    case url of
      Home              -> methodM GET >> renderFunction >>= (ok . toResponse . hamletTest)
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
