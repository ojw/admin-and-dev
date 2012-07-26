{-# LANGUAGE DeriveDataTypeable, GeneralizedNewtypeDeriving, TemplateHaskell, 
  TypeOperators, OverloadedStrings, QuasiQuotes, NoMonomorphismRestriction #-}

module Sitemap where

import Prelude                 hiding ((.))
import Control.Category        (Category((.)))
import Web.Routes.TH           (derivePathInfo)
import Data.Data               (Data, Typeable)
import Data.Text               (Text)
import Web.Routes              (PathInfo)
import Web.Routes.Boomerang    ((<>), lit, (</>), anyText, (:-), Router, xmaph, int)
import Text.Boomerang.TH       (derivePrinterParsers)

newtype UserId
    = UserId { unUserId :: Int }
      deriving (Eq, Ord, Enum, Read, Show, Data, Typeable, PathInfo)

data Sitemap
    = Home
    | Profile UserId
    | Echo Text
      deriving (Eq, Ord, Read, Show, Data, Typeable)

$(derivePrinterParsers ''Sitemap)

sitemap :: Router () (Sitemap :- ())
sitemap =
    (  rHome
    <> rProfile . (lit "profile" </> userId)
    <> rEcho . (lit "message" </> anyText)
    )
    where userId :: Router () (UserId :- ())
          userId = 
              xmaph UserId (Just . unUserId) int
