{-# LANGUAGE FlexibleContexts, OverloadedStrings, GeneralizedNewtypeDeriving #-}

module Framework.Auth.Types.Error where

import Control.Monad.Error

data AuthError
    = UserNameNotAvailable
    | EmailNotAvailable
    | IncorrectUserNameOrPassword
    | UserDoesNotExist
    | PasswordHashFailed
    | DefaultAuthError
    | InvalidAuthToken

instance Error AuthError where
    noMsg = DefaultAuthError
