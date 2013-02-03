{-# LANGUAGE TemplateHaskell, DeriveDataTypeable #-}

module Framework.Auth.Internal.Types.AuthState where

import Data.SafeCopy
import Data.Lens.Template
import Data.Data

import Framework.Auth.Internal.Types.UserPassword
import Framework.Auth.Internal.Types.UserToken

data AuthState = AuthState
    { _userPasswords    :: UserPasswords
    , _userTokens       :: UserTokens
    } deriving (Ord, Eq, Read, Show, Data, Typeable)

$(makeLens ''AuthState)
$(deriveSafeCopy 0 'base ''AuthState)
