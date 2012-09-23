{-# LANGUAGE DeriveDataTypeable, FlexibleContexts, GeneralizedNewtypeDeriving
  , MultiParamTypeClasses, OverloadedStrings, ScopedTypeVariables
  , TemplateHaskell, TypeFamilies, FlexibleInstances, RecordWildCards
  , TypeOperators #-}

module Plugins.Profile.Api where

import Prelude hiding ( (.) )
import Control.Category ( (.) )
import Control.Monad.Trans
import Data.Data
import Data.Acid
import Data.Acid.Advanced
import Data.Aeson
import Data.Text as Text
import Data.Text.Encoding   ( decodeUtf8 )

import Happstack.Server -- ( Happstack, Response )
import Web.Routes.Happstack

import Text.Boomerang.TH
import Web.Routes -- ( Site, runRouteT )
import Web.Routes.Boomerang

import Control.Monad ( mzero )
import Data.ByteString.Lazy as L

import Happstack.Auth as A
import Plugins.Auth
import Util.HasAcidState
import Util.GetBody
import Plugins.Profile.Acid as P

-- URL routing for the Profile API

data ProfileAPIURL
    = ProfileAPIDefault -- probably won't actually use routing beyond this
    | SecondOptionToSuppressPointlessWarningBecauseBoomerangIsMorePowerThanINeedHere

$(derivePrinterParsers ''ProfileAPIURL)

profileAPIBoomerang :: Router () (ProfileAPIURL :- ())
profileAPIBoomerang =
    (  rProfileAPIDefault
    )

-- maps a URL in the Profile API to a response
-- need function to get json Value from request body
processProfileURL :: (HasAcidState m A.ProfileState, HasAcidState m AuthState, HasAcidState m P.ProfileState, Happstack m) 
               => ProfileAPIURL -> RouteT ProfileAPIURL m Response
processProfileURL url =
    do
        body <- lift $ getBody
        mUserId <- lift $ getUserId'
        case mUserId of
            Nothing     -> ok $ toResponse $ ("You aren't logged in!" :: String) -- improve, obv
            Just uid    -> do case decode body :: Maybe MaybeProfile of
                                Nothing         -> ok $ toResponse $ ("Yeah that request body didn't have the right stuff." :: String)
                                Just request    -> do t <- lift $ runProfileAPI uid request
                                                      ok $ toResponse $ t

profileAPISite :: (Happstack m, HasAcidState m A.ProfileState, HasAcidState m AuthState, HasAcidState m P.ProfileState) 
            => Site ProfileAPIURL (m Response)
profileAPISite = boomerangSite (runRouteT processProfileURL) profileAPIBoomerang

------------------------------------------------------------------

runProfileAPI :: (HasAcidState m P.ProfileState, MonadIO m, Happstack m) 
           => UserId -> MaybeProfile -> m Response -- Text
runProfileAPI uid maybeProfile =
    do  profileState :: AcidState P.ProfileState <- getAcidState
        res <- update' profileState $ UpdateProfile uid maybeProfile
        case res of
            Nothing -> ok $ toResponse $ ("Wtf, something went wrong" :: Text) -- should not be "ok"
            Just p  -> ok $ toResponse $ encode p
