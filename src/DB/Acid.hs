module DB.Acid where

import Data.Acid

import Framework.Location
import Framework.Profile
import Framework.Auth

data Acid = Acid
    { authState :: AcidState AuthState
    , profileState :: AcidState ProfileState
    , locationState :: AcidState LocationState
    } 
