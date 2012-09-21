{-# LANGUAGE OverloadedStrings #-}

module Main 

where

import Happstack.Server             ( simpleHTTP, nullConf )
import Data.Text                    ( Text )

import Acid
import App
import Site.Routes



main :: IO ()
main = withAcid Nothing $ \acid -> simpleHTTP nullConf $ runApp acid (app "http://localhost:8000" "/api")
