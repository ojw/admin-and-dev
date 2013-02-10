module Framework.View where

import Framework.Location
import Framework.Auth

data FrameworkView
    = FrameworkView 
    | FWLocView LocationView
    | FWAuthView AuthView

