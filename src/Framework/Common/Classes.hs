{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies #-}

module Framework.Common.Classes where

class View obj view m | obj -> m where
    view    :: obj -> m view

class Holder val key holder where
    add     :: val -> holder -> (key, holder)
    modify  :: key -> (val -> val) -> holder -> (val, holder)
    delete  :: key -> holder -> holder 

class Create obj options where
    new     :: options -> obj 
