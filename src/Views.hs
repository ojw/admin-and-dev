{-# LANGUAGE DeriveDataTypeable, GeneralizedNewtypeDeriving, TemplateHaskell, 
  TypeOperators, OverloadedStrings, QuasiQuotes, NoMonomorphismRestriction #-}

module Views where

import Data.Text               ( Text )
import Text.Hamlet             ( hamlet )
import Text.Lucius             ( lucius )

import Sitemap ( Sitemap(..) )

hamletTest = [hamlet| 
                $doctype 5
                <html>
                  <head>
                    <link href="/site.css" rel="stylesheet" media="all" type="text/css">
                  <body>
                    <div class="container">
                      <p>"Does it... does it really work???"
                      <p><a href=@{Home}>Home</a>
             |]
