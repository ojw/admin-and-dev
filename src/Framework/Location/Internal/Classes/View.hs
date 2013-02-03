{-# LANGUAGE MultiParamTypeClasses #-}

module Framework.Location.Internal.Classes.View where

import Framework.Location.Internal.Types.Location

class View obj view where
    view :: (LocationAction m) => obj -> m view
