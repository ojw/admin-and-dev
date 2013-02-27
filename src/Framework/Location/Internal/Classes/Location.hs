
{-# LANGUAGE GeneralizedNewtypeDeriving, DeriveDataTypeable, TemplateHaskell, 
    MultiParamTypeClasses, FlexibleContexts, FlexibleInstances #-}

module Framework.Location.Internal.Classes.Location where

import Framework.Location.Internal.Types.Chat      ( Chat )
import Framework.Location.Internal.Types.Location  ( LocationId, LocationAction )

class Loc l where
    canJoin     :: l -> LocationAction Bool
    onJoin      :: l -> LocationAction ()
    canLeave    :: l -> LocationAction Bool
    onLeave     :: l -> LocationAction ()
    exit        :: l -> LocationAction LocationId
    chat        :: Chat -> l -> LocationAction ()
