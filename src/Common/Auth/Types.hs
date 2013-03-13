{-# LANGUAGE TemplateHaskell, DeriveDataTypeable, FlexibleContexts, OverloadedStrings,
    GeneralizedNewtypeDeriving, MultiParamTypeClasses, FunctionalDependencies, FlexibleInstances,
    UndecidableInstances, TypeFamilies, ScopedTypeVariables #-}

module Common.Auth.Types where

import Control.Monad.Error
import Data.Functor
import Data.Text
import Data.SafeCopy
import Control.Lens
import Data.Data
import Data.Acid
import Data.IxSet
import System.Entropy
import Data.Monoid
import Data.ByteString.Char8 ( ByteString )
import Crypto.BCrypt

import Util.HasAcidState
import Framework.Profile

data AuthError
    = IncorrectUserNameOrPassword
    | UserDoesNotExist
    | PasswordHashFailed
    | DefaultAuthError
    | InvalidAuthToken
    | AuthProfileError ProfileError

newtype PlainPass = PlainPass { unPlainPass :: ByteString } deriving (Ord, Eq, Read, Show)
newtype HashedPass = HashedPass { unHashedPass :: ByteString } deriving (Ord, Eq, Read, Show, Data, Typeable, SafeCopy)

data UserPassword = UserPassword
    { _upwUserId   :: UserId
    , _upwPassword :: HashedPass
    } deriving (Ord, Eq, Read, Show, Data, Typeable)

inferIxSet "UserPasswords" ''UserPassword 'noCalcs [''UserId, ''HashedPass]

newtype AuthToken = AuthToken ByteString deriving (Ord, Eq, Read, Show, Data, Typeable, SafeCopy)

data UserToken = UserToken
    { _utUserId       :: UserId
    , _utAuthToken    :: Maybe AuthToken
    } deriving (Ord, Eq, Read, Show, Data, Typeable)

inferIxSet "UserTokens" ''UserToken 'noCalcs [''UserId, ''AuthToken]

data AuthState = AuthState
    { _asUserPasswords    :: UserPasswords
    , _asUserTokens       :: UserTokens
    } deriving (Ord, Eq, Read, Show, Data, Typeable)

data AuthView
    = AuthTokenView AuthToken
    | AuthViewSuccess Bool

makeFields ''UserPassword
makeFields ''UserToken
makeFields ''AuthState

getHashedPass'' :: UserPasswords -> UserId -> Maybe HashedPass
getHashedPass'' userPasswords userId = fmap (view password) $ getOne $ userPasswords @= userId

generateAuthToken :: MonadIO m => m AuthToken
generateAuthToken = do
    randomPart <- liftIO $ getEntropy 64
    return $ AuthToken $ mappend "FOO" randomPart 
