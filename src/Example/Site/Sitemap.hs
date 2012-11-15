{-# LANGUAGE DeriveDataTypeable, TemplateHaskell, TypeOperators, OverloadedStrings #-}

module Example.Site.Sitemap 

where

import Prelude hiding               ( (.) )
import Control.Category             ( (.) )
import Data.Data                    ( Data, Typeable )
import Text.Boomerang.TH            ( derivePrinterParsers )
import Web.Routes.Boomerang         ( (<>), lit, (</>), anyText, (:-), Router
                                    , xmaph, int, boomerangSite, integer )
data Sitemap
    = Home
    | Login
    | Logout
    | Create
      deriving (Eq, Ord, Read, Show, Data, Typeable)

$(derivePrinterParsers ''Sitemap)

sitemap :: Router () (Sitemap :- ())
sitemap =
    (  rHome
    <> rCreate . (lit "create")
    <> rLogin  . (lit "login")
    <> rLogout  . (lit "logout")
    ) 
