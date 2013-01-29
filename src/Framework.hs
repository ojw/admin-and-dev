

module Framework where

import Control.Monad.State

import Framework.Location

data FrameworkState = FrameworkState
    { locationState :: LocationState 
    }

type FrameworkAction = State FrameworkState

data FrameworkApi
    = FWLocApi LocationApi

data FrameworkView = FrameworkView 

runApi :: FrameworkApi -> FrameworkAction FrameworkView
runApi (FWLocApi locationApi) = return FrameworkView
