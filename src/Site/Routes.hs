{-# LANGUAGE OverloadedStrings #-}

module Site.Routes 

where

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
import Site.Sitemap
import Core.Auth
import Acid
import App
import Api
import Site.Handlers

import Core.Room.Api

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
        msum [ dir "favicon.ico" $ notFound (toResponse ())
             , apiSite  baseURL apiDir
             , dir "js" $ serveFile (asContentType "text/javascript") "Core/Room/Scripts/room.js"
             , dir "css" $ serveFile (asContentType "text/css") "Core/Room/Scripts/room.css"
             , decodeBody (defaultBodyPolicy "/tmp/" 0 1000 1000) >> implSite baseURL "" site
             , seeOther ("" :: String) (toResponse ())
             ] 
