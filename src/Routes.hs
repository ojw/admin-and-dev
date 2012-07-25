{-# LANGUAGE DeriveDataTypeable, GeneralizedNewtypeDeriving, TemplateHaskell, 
  TypeOperators, OverloadedStrings #-}

module Routes where

import Prelude                 hiding (head, id, (.))
import Control.Category        (Category(id, (.)))
import Control.Monad           (msum) 
import Web.Routes.TH           (derivePathInfo)
import Data.Data               (Data, Typeable)
import Data.Text               (Text, unpack)
import Web.Routes              ( PathInfo(..), RouteT, showURL
                               , runRouteT, Site(..), setDefault, mkSitePI)
import Happstack.Server        ( Response, ServerPartT, ok, toResponse, simpleHTTP
                               , nullConf, seeOther, dir, notFound, seeOther)
import Web.Routes.Boomerang
import Text.Boomerang.TH       (derivePrinterParsers)
import Web.Routes.Happstack    (implSite)

newtype UserId
    = UserId { unUserId :: Int }
      deriving (Eq, Ord, Enum, Read, Show, Data, Typeable, PathInfo)

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

userId :: Router () (UserId :- ())
userId =
    xmaph UserId (Just . unUserId) int

route :: Sitemap -> RouteT Sitemap (ServerPartT IO) Response
route url =
    case url of
      Home              -> ok $ toResponse ("Home" :: String)
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
