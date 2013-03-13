{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies #-}

module Common.Classes where

import Control.Monad.State ( MonadState, get, put )

class View obj view m | obj -> m where
    view    :: obj -> m view

class IndexedContainer index value container | container -> index value where
    add     :: value -> container -> (index, container)
    modify  :: index -> (value -> value) -> container -> (Maybe value, container)
    delete  :: index -> container -> container 

add' :: (IndexedContainer index value container, MonadState container m) => value -> m index
add' value = do
    container <- get
    let (index, container') = add value container
    put container'
    return index

modify' :: (IndexedContainer index value container, MonadState container m) => index -> (value -> value) -> m (Maybe value)
modify' index f = do
    container <- get
    let (value, container') = modify index f container
    put container'
    return value

delete' :: (IndexedContainer index value container, MonadState container m) => index -> m ()
delete' index = do
    container <- get
    put $ delete index container

add'' :: IndexedContainer index value container => value -> container -> container
add'' value container = snd $ add value container

modify'' :: IndexedContainer index value container => index -> (value -> value) -> container -> container
modify'' index f container = snd $ modify index f container

delete'' :: IndexedContainer index value container => index -> container -> container
delete'' = delete

class Create options object | options -> object, object -> options where
    blank   :: object
    update  :: options -> object -> object

create :: Create options object => options -> object
create = flip update blank
