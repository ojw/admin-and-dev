{-# LANGUAGE OverloadedStrings #-}

module Main 

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

import Acid
import App
import Api

import Site.Routes

main :: IO ()
main = withAcid Nothing $ \acid -> simpleHTTP nullConf $ runApp acid (app "http://localhost:8000" "/api")
