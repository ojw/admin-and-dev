{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies #-}

module Framework.Common.Classes where

import Control.Monad.State ( MonadState, get, put )

class View obj view m | obj -> m where
    view    :: obj -> m view

class IndexedContainer index value container | container -> index value where
    add     :: value -> container -> (index, container)
    modify  :: index -> (value -> value) -> container -> (value, container)
    delete  :: index -> container -> container 

add' :: (IndexedContainer index value container, MonadState container m) => value -> m index
add' value = do
    container <- get
    let (index, container') = add value container
    put container'
    return index

modify' :: (IndexedContainer index value container, MonadState container m) => index -> (value -> value) -> m value
modify' index f = do
    container <- get
    let (value, container') = modify index f container
    put container'
    return value

delete' :: (IndexedContainer index value container, MonadState container m) => index -> m ()
delete' index = do
    container <- get
    put $ delete index container

class Create obj options where
    new     :: options -> obj 
