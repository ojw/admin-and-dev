

module Framework.Common.Types where

import Control.Monad.State
import Control.Monad.Reader

import Framework.Location.Internal.Types.Location

data FrameworkState = FrameworkState
    { locationState :: LocationState 
    }

type FrameworkAction = State FrameworkState

type FrameworkReader = Reader FrameworkState 
