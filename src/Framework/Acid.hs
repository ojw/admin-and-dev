module Framework.Acid where

import Data.Acid

import Framework.Location
import Framework.Profile
import Framework.Auth

data Acid = Acid
    { authState :: AuthState
    , profileState :: ProfileState
    , locationState :: AcidState LocationState
    } 
