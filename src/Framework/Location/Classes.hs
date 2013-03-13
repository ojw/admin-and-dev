
{-# LANGUAGE GeneralizedNewtypeDeriving, DeriveDataTypeable, TemplateHaskell, 
    MultiParamTypeClasses, FlexibleContexts, FlexibleInstances #-}

module Framework.Location.Classes where

import DB.Location.LocationAction ( LocationAction )
import Common.Location.Types ( Chat, LocationId )

class Loc l where
    canJoin     :: l -> LocationAction Bool
    onJoin      :: l -> LocationAction ()
    canLeave    :: l -> LocationAction Bool
    onLeave     :: l -> LocationAction ()
    exit        :: l -> LocationAction LocationId
    chat        :: Chat -> l -> LocationAction ()
