module Framework.Error where

import Control.Monad.Error

import Framework.Location
import Framework.Auth

data FrameworkError
    = FWLocError LocationError
    | FWAuthError AuthError
    | DefaultError
    | UserNotLoggedIn

instance Error FrameworkError where
    noMsg = DefaultError

