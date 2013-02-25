module Framework.Acid where

import Framework.Location
import Framework.Profile
import Framework.Auth

data Acid = Acid
    { authState :: AuthState
    , profileState :: ProfileState
    , locationState :: LocationState
    } 
