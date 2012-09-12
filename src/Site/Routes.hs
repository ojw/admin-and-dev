{-# LANGUAGE OverloadedStrings #-}

module Site.Routes 

where

import Control.Monad.Trans          ( liftIO )
import Control.Monad                ( mzero )
import Control.Monad                ( msum )
import Happstack.Server.RqData      ( decodeBody, defaultBodyPolicy )
import Happstack.Server             ( Response, toResponse, simpleHTTP, nullConf
                                    , seeOther, dir, notFound, seeOther, serveDirectory
                                    , serveFile, guessContentTypeM, asContentType )
import Happstack.Server
import Web.Routes                   ( RouteT, runRouteT, Site(..), setDefault )

import Data.Text                    ( Text )
import Web.Routes.Boomerang         ( boomerangSite )
import Web.Routes.Happstack         ( implSite, implSite_ )

import Util.HasAcidState
import Site.Sitemap
import Plugins.Auth
import Acid
import App
import API
import Site.Handlers

import Plugins.Room.API

route :: Sitemap -> RouteT Sitemap App Response
route url =
    case url of
        Home   -> homePage
        Login   -> loginPage
        Logout   -> logoutPage
        Create -> createPage

site :: Site Sitemap (App Response)
site = setDefault Home $ boomerangSite (runRouteT route) sitemap

app :: Text -> Text -> App Response
app baseURL apiDir =  
    do  decodeBody (defaultBodyPolicy "/tmp/" 0 1000 1000)
        msum [ dir "favicon.ico" $ notFound (toResponse ())
             , implSite baseURL "" site
             , apiSite  baseURL apiDir
             , dir "static" $ serveFile (asContentType "text/javascript") "Plugins/Room/room.js"
             , seeOther ("" :: String) (toResponse ())
             ] 
