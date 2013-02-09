

module Framework where

import Control.Monad.State

import Framework.Location
import Framework.Location.Internal.Views.LocationView
import Framework.Auth
import Framework.Profile

data FrameworkApi
    = FWLocApi LocationApi
    | FWAuthApi AuthApi
    | FWProfileApi ProfileApi

data FrameworkView
    = FrameworkView 
    | FWLocView LocationView
    | FWAuthView AuthView
    | FWProfileView ProfileView

class (Functor m, Monad m) => MonadFrameworkAction m

runApi :: MonadFrameworkAction m => FrameworkApi -> m FrameworkView
runApi (FWLocApi locationApi) = return FrameworkView
runApi (FWAuthApi authApi) = return FrameworkView
runApi (FWProfileApi profileApi) = return FrameworkView
