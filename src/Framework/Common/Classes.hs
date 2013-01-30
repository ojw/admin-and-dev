{-# LANGUAGE MultiParamTypeClasses #-}

module Framework.Common.Classes where

-- many types (+ecosystems) will implement these three classes
-- locations, games, and profiles at least
-- (and that's the entire framework minus auth)

class View obj view where
    view    :: obj -> view

class Persistent val key holder where
    add     :: val -> holder -> (key, holder)
    modify  :: key -> (val -> val) -> holder -> holder
    delete  :: key -> holder -> holder 

class Create obj options where
    new     :: options -> FrameworkReader obj 
