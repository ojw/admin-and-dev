{-# LANGUAGE DeriveDataTypeable, TemplateHaskell, TypeOperators, OverloadedStrings #-}

module Sitemap 

where

import Prelude hiding               ( (.) )
import Control.Category             ( (.) )
import Data.Data                    ( Data, Typeable )
import Text.Boomerang.TH            ( derivePrinterParsers )
import Web.Routes.Boomerang         ( (<>), lit, (</>), anyText, (:-), Router
                                    , xmaph, int, boomerangSite, integer )
{-
data APImap
    = Auth
    | Room
--    | Game
--    | Profile
      deriving (Eq, Ord, Read, Show, Data, Typeable)


$(derivePrinterParsers ''APImap)
-}

data Sitemap
    = Home
    | Login
    | Logout
    | Create
  --  | API APImap
      deriving (Eq, Ord, Read, Show, Data, Typeable)

$(derivePrinterParsers ''Sitemap)

sitemap :: Router () (Sitemap :- ())
sitemap =
    (  rHome
    <> rCreate . (lit "create")
    <> rLogin  . (lit "login")
    <> rLogout  . (lit "logout")
--    <> rAPI . rAuth . (lit "api" <> lit "auth")
--    <> rAPI . rRoom . (lit "api" <> lit "room")
    ) 
