

module Framework.Common.Types where

import Control.Monad.State
import Control.Monad.Reader

import Framework.Profile
import Framework.Location.Internal.Types.Location

data FrameworkState = FrameworkState
    { _locationState :: LocationState 
    , _profileState  :: ProfileState
    }

type FrameworkAction = State FrameworkState

type FrameworkReader = Reader FrameworkState 
