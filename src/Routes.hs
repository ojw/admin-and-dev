{-# LANGUAGE TemplateHaskell #-}

module Routes where

import State (UserId)

import Web.Routes.TH           (derivePathInfo)

data SiteMap
    = Home
    | Profile UserId
    | Adder Int Int

$(derivePathInfo ''UserId)

$(derivePathInfo ''SiteMap)
