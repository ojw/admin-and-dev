
{-# LANGUAGE GeneralizedNewtypeDeriving, DeriveDataTypeable, TemplateHaskell, 
    MultiParamTypeClasses, FlexibleContexts, FlexibleInstances #-}

module Framework.Location.Internal.Classes.Location where

import Framework.Location.Internal.Types.Chat      ( Chat )
import Framework.Location.Internal.Types.Location  ( LocationId, LocationAction )

class (LocationAction p m) => Location l p m where
    canJoin     :: l -> m Bool
    onJoin      :: l -> m ()
    canLeave    :: l -> m Bool
    onLeave     :: l -> m ()
    exit        :: l -> m LocationId
    chat        :: Chat -> l -> m ()
