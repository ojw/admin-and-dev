
{-# LANGUAGE GeneralizedNewtypeDeriving, DeriveDataTypeable, TemplateHaskell, 
    MultiParamTypeClasses, FlexibleContexts, FlexibleInstances #-}

module Framework.Location.Internal.Classes.Location where

import Framework.Location.Internal.Types.Chat      ( Chat )
import Framework.Location.Internal.Types.Location  ( LocationId, LocationAction )

class Loc l where
    canJoin     :: (LocationAction m) => l -> m Bool
    onJoin      :: (LocationAction m) => l -> m ()
    canLeave    :: (LocationAction m) => l -> m Bool
    onLeave     :: (LocationAction m) => l -> m ()
    exit        :: (LocationAction m) => l -> m LocationId
    chat        :: (LocationAction m) => Chat -> l -> m ()
