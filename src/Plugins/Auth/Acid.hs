{-# LANGUAGE DeriveDataTypeable, FlexibleContexts, GeneralizedNewtypeDeriving
  , MultiParamTypeClasses, OverloadedStrings, ScopedTypeVariables
  , TemplateHaskell, TypeFamilies, FlexibleInstances, RecordWildCards
  , TypeOperators #-}

module Plugins.Auth.Acid

( UserId(..)
, AuthState
, initialAuthState
, ProfileState
, initialProfileState
, getUserId'
, getUserId
)

where

import Control.Monad.Trans          ( MonadIO )
import Happstack.Auth.Core.Auth
import Happstack.Auth.Core.Profile
import Data.Acid.Advanced

import Happstack.Server             ( Happstack, ok, toResponse, Response )
import Data.Acid                    ( AcidState )

import Data.Text
import Data.Set                     (Set)
import qualified Data.Set as Set

import Util.HasAcidState

getUserId' :: (HasAcidState m AuthState, HasAcidState m ProfileState, Happstack m) => m (Maybe UserId)
getUserId' =
    do
        authState :: AcidState AuthState <- getAcidState
        profileState :: AcidState ProfileState <- getAcidState
        getUserId authState profileState


tryLogin :: (MonadIO m, HasAcidState m AuthState, Happstack m) => Text -> Text -> m Response -- (Either AuthTemplateError UserPassId)
tryLogin username password =
               do authStateH :: AcidState AuthState <- getAcidState
                  r <- query' authStateH (CheckUserPass username password)
                  case r of
                    (Left e) -> ok $ toResponse ("Authentication Failed!" :: String) -- (Left $ UPE e)
                    (Right userPassId) -> 
                        do authId <- do authIds <- query' authStateH (UserPassIdAuthIds userPassId)
                                        case Set.size authIds of
                                             1 -> return (Just $ Prelude.head $ Set.toList $ authIds)
                                             n -> return Nothing
                           addAuthCookie authStateH authId (AuthUserPassId userPassId)
                           ok $ toResponse $ show userPassId-- seeOther onAuthURL (toResponse ())
