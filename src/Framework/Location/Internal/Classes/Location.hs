
{-# LANGUAGE GeneralizedNewtypeDeriving, DeriveDataTypeable, TemplateHaskell, 
    MultiParamTypeClasses, FlexibleContexts, FlexibleInstances #-}

module Framework.Location.Internal.Classes.Location where

import Framework.Location.Internal.Types.Chat      ( Chat )
import Framework.Location.Internal.Types.Location  ( LocationId, LocationAction )

class Location l where
    canJoin     :: (LocationAction p m) => l -> m Bool
    onJoin      :: (LocationAction p m) => l -> m ()
    canLeave    :: (LocationAction p m) => l -> m Bool
    onLeave     :: (LocationAction p m) => l -> m ()
    exit        :: (LocationAction p m) => l -> m LocationId
    chat        :: (LocationAction p m) => Chat -> l -> m ()
