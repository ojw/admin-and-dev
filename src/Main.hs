{-# LANGUAGE OverloadedStrings #-}

module Main 

where

import Happstack.Server ( simpleHTTP, nullConf )
import Data.Text        ( Text )

import Acid             ( Acid, withAcid )
import App              ( App, runApp )
import Site.Routes      ( app )



main :: IO ()
main = withAcid Nothing $ \acid -> simpleHTTP nullConf $ runApp acid $ app "http://localhost:8000"
