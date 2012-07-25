{-# LANGUAGE DeriveDataTypeable, GeneralizedNewtypeDeriving, TemplateHaskell, 
  TypeOperators, OverloadedStrings, QuasiQuotes, NoMonomorphismRestriction #-}

module Views where

import Control.Monad           (msum, liftM)
import Data.Text               (Text, pack, unpack, reverse, toUpper)
import Text.Hamlet
import Text.Lucius
import Happstack.Server        ( Response, ServerPartT, ok, toResponse, simpleHTTP
                               , nullConf, seeOther, dir, notFound, seeOther)
import Web.Routes              ( PathInfo(..), RouteT, showURL
                               , runRouteT, Site(..), setDefault, mkSitePI
                               , MonadRoute, URL, askRouteFn)

import Sitemap (Sitemap(..))

-- if there's a convenient way to make the overall template 
-- pass the renderer to the individual components that might be nice
-- or it might be unnecessary and I should do as Tazjin did

-- need overall template for site that individual page bits will be inside of

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
