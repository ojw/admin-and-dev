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
                               , nullConf, seeOther, dir, notFound, seeOther)
import Web.Routes.Boomerang
import Text.Boomerang.TH       (derivePrinterParsers)
import Web.Routes.Happstack    (implSite)
import Text.Hamlet
import Text.Lucius

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
      Home              -> hamletTest
      (Profile userId)  -> ok $ toResponse $ "Profile" ++ show (unUserId userId)
      (Echo message)    -> ok $ toResponse $ "Message" ++ unpack message


-- from Tazjin's blog
convRender :: (url -> [(Text, Maybe Text)] -> Text)
           -> (url -> [(Text, Text)]-> Text)
convRender maybeF = 
  (\url params -> maybeF url $ map (\(t1, t2) -> (t1, Just t2)) params)

-- from Tazjin's blog
renderFunction :: MonadRoute m => m (URL m -> [(Text, Text)] -> Text)
renderFunction = liftM convRender $ askRouteFn

--hamletTest :: UrlHtml Sitemap
--actually apparently it's (MonadRoute m, FilterMonad Response m, URL m ~ Sitemap) => m Response
--so... hmm
-- in future, write all handlers to take acid and renderer, template can then pass it to internal bits?
hamletTest = do
                renderer <- renderFunction
                ok $ toResponse $ [hamlet| 
                    $doctype 5
                    <html>
                      <head>
                        <link href="/site.css" rel="stylesheet" media="all" type="text/css">
                      <body>
                        <div class="container">
                          <p>"Does it... does it really work???"
                          <p><a href=@{Home}>Home</a>
                    |] renderer


site :: Site Sitemap (ServerPartT IO Response)
site =
       setDefault Home $ boomerangSite (runRouteT route) sitemap

main :: IO ()
main = simpleHTTP nullConf $
       msum [ dir "favicon.ico" $ notFound (toResponse ())
            , implSite "http://localhost:8000" "/route" site
            , seeOther ("/route/" :: String) (toResponse ())
            ] 
