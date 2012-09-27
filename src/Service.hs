{-# LANGUAGE Rank2Types #-}

module Service where

import Util.HasAcidState
import Data.ByteString.Lazy
import Happstack.Server

-- It can store some stuff, and it can use that stuff to provide a response
data StatefulService state = StatefulService
    { initialState  :: state -- what we stick in the Acid type
    , handler       :: (HasAcidState m state) => ByteString -> m Response -- takes ByteString, probably parses into json, does some stuff and returns a response
    }

-- could have
-- authService :: StatefulService AuthState
-- profileService :: StatefulService Profile State
-- ...

-- acid will be initialized with (initialState authService), etc
-- handler will call (handler authService), etc
