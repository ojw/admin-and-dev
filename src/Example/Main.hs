{-# LANGUAGE OverloadedStrings #-}

module Example.Main 

where

import Happstack.Server ( simpleHTTP, nullConf )
import Data.Text        ( Text )

import Example.Acid             ( Acid, withAcid )
import Example.App              ( App, runApp )
import Example.Site.Routes      ( app )



main :: IO ()
main = withAcid Nothing $ \acid -> simpleHTTP nullConf $ runApp acid $ app "http://localhost:8000"
