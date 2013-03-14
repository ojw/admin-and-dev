module Framework.View where

import Framework.Location
import Framework.Auth
import Framework.Error

data FrameworkView
    = FrameworkView 
    | FWLocView LocationView
    | FWAuthView AuthView
    | FWError FrameworkError 
