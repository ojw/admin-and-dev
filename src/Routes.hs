{-# LANGUAGE TemplateHaskell, DeriveDataTypeable, GeneralizedNewtypeDeriving
           , TypeOperators #-}

module Routes where

import State (UserId)

import Web.Routes.TH           (derivePathInfo)
import Data.Data            (Data, Typeable)

import Prelude                 hiding (head)

import Control.Monad           (msum)
import Data.Data               (Data, Typeable)
import Data.Monoid             (mconcat)
import Data.Text               (pack)
import Happstack.Server        ( Response, ServerPartT, ok, toResponse, simpleHTTP
                               , nullConf, seeOther, dir, notFound, seeOther)
import Text.Blaze.Html4.Strict ( (!), html, head, body, title, p, toHtml
                               , toValue, ol, li, a)
import Text.Blaze.Html4.Strict.Attributes (href)
import Web.Routes              ( PathInfo(..), RouteT, showURL
                               , runRouteT, Site(..), setDefault, mkSitePI)
import Web.Routes.TH           (derivePathInfo)
import Web.Routes.Happstack    (implSite)


data Sitemap
    = Home
    | Profile UserId
    | Foo
--    | Adder Int Int
    deriving (Eq, Ord, Read, Show, Data, Typeable)

$(derivePathInfo ''UserId)
$(derivePathInfo ''Sitemap)

-- will later take acid handle
route :: Sitemap -> RouteT Sitemap (ServerPartT IO) Response
route url =
    case url of
      Home              -> homePage
      (Profile userId)  -> profilePage userId
      Foo               -> fooPage
      --(Adder x1 x2)     -> adderPage x1 x2

homePage :: RouteT Sitemap (ServerPartT IO) Response
homePage = 
       ok $ toResponse $
          html $ do
            head $ title $ (toHtml "Welcome Home!")


profilePage :: UserId -> RouteT Sitemap (ServerPartT IO) Response
profilePage userId =
    do homeURL <- showURL Home
       ok $ toResponse $
          html $ do
            head $ title $ (toHtml $ "User " ++ show userId)
            body $ do
                   p $ toHtml $ "You are looking at the page of " ++ show userId
                   p $ do toHtml "Click "
                          a ! href (toValue homeURL) $ toHtml "here"
                          toHtml " to return home."

fooPage :: RouteT Sitemap (ServerPartT IO) Response
fooPage = 
      ok $ toResponse $ 
         html $ do head $ title $ (toHtml "FOOOO")

site :: Site Sitemap (ServerPartT IO Response)
site =
       setDefault Home $ mkSitePI (runRouteT route) 

main :: IO ()
main = simpleHTTP nullConf $
       msum [ dir "favicon.ico" $ notFound (toResponse ())
            , implSite (pack "http://localhost:8000") (pack "/route") site
            , seeOther "/route" (toResponse ())
            ] 
