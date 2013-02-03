{-# LANGUAGE MultiParamTypeClasses #-}

module Framework.Common.Classes where

-- many types (+ecosystems) will implement these three classes
-- locations, games, and profiles at least
-- maybe auth?

import Framework.Location.Internal.Types.Location

class LocationAction m => FrameworkAction m

class View obj view where
    view    :: FrameworkAction m => obj -> m view

class Holder val key holder where
    add     :: val -> holder -> (key, holder)
    modify  :: key -> (val -> val) -> holder -> holder
    delete  :: key -> holder -> holder 

class Create obj options where
    new     :: options -> obj 
