
{-# LANGUAGE GeneralizedNewtypeDeriving, DeriveDataTypeable, TemplateHaskell, 
    MultiParamTypeClasses, FlexibleContexts, FlexibleInstances #-}

module Framework.Location.Internal.Classes.Location where

import Framework.Location.Internal.Types.Chat      ( Chat )
import Framework.Location.Internal.Types.Location  ( LocationId, MonadLocationAction )

class Loc l where
    canJoin     :: (MonadLocationAction m) => l -> m Bool
    onJoin      :: (MonadLocationAction m) => l -> m ()
    canLeave    :: (MonadLocationAction m) => l -> m Bool
    onLeave     :: (MonadLocationAction m) => l -> m ()
    exit        :: (MonadLocationAction m) => l -> m LocationId
    chat        :: (MonadLocationAction m) => Chat -> l -> m ()
