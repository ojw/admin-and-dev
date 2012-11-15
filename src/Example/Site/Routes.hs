{-# LANGUAGE OverloadedStrings #-}

module Example.Site.Routes 

where

import Control.Monad.Trans
import Control.Monad                ( msum )
import Happstack.Server.RqData      ( decodeBody, defaultBodyPolicy )
import Happstack.Server             ( Response, toResponse, simpleHTTP, nullConf
                                    , seeOther, dir, notFound, seeOther, serveDirectory
                                    , serveFile, guessContentTypeM, asContentType )
import Happstack.Server
import Web.Routes                   ( RouteT, runRouteT, Site(..), setDefault )
import Data.Text                    ( Text )
import Web.Routes.Boomerang         ( boomerangSite )
import Web.Routes.Happstack         ( implSite )

import Util.HasAcidState
import Example.Site.Sitemap
import Server.Auth.Auth
import Example.Acid
import Example.App
import Example.Service
import Example.Site.Handlers

--import Server.Room.Api

route :: Sitemap -> RouteT Sitemap App Response
route url =
    case url of
        Home   -> homePage
        Login   -> loginPage
        Logout   -> logoutPage
        Create -> createPage

site :: Site Sitemap (App Response)
site = setDefault Home $ boomerangSite (runRouteT route) sitemap

app :: Text -> App Response
app baseURL =
        msum [ dir "favicon.ico" $ serveFile (asContentType "image") "favicon.ico"
             , dir "api" $ routeService
             , dir "foo" $ ok $ toResponse ("Bar." :: Text)
             , dir "js" $ serveFile (asContentType "text/javascript") "Server/Room/Scripts/room.js"
             , dir "css" $ serveFile (asContentType "text/css") "Server/Room/Scripts/room.css"
             , decodeBody (defaultBodyPolicy "/tmp/" 0 1000 1000) >> implSite baseURL "" site
             , seeOther ("" :: String) (toResponse ())
             ] 
