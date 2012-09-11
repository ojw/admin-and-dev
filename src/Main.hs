{-# LANGUAGE OverloadedStrings #-}

module Main 

where

import Control.Monad                ( msum )
import Happstack.Server.RqData      ( decodeBody, defaultBodyPolicy )
import Happstack.Server             ( Response, toResponse, simpleHTTP, nullConf
                                    , seeOther, dir, notFound, seeOther
                                    )
import Web.Routes                   ( RouteT, runRouteT, Site(..), setDefault
                                    )
import Web.Routes.Boomerang         ( boomerangSite )
import Web.Routes.Happstack         ( implSite )

import Util.HasAcidState
import Site.Sitemap
import Plugins.Auth
import Acid
import App
import Site.Handlers

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
