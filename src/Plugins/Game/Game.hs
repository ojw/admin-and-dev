{-# LANGUAGE MultiParamTypeClasses #-}

module Game where

import Data.Aeson       ( ToJSON, FromJSON )
import Data.Acid
import Data.SafeCopy    ( SafeCopy )

import Util.HasAcidState

class (ToJSON o, FromJSON o) => Options o
class FromJSON c => Command c

-- not sure this is right
class (SafeCopy state, Options options) => GameState state options  where
    --getState    :: (HasAcidState m state) => m state
    getState    :: state
    newGame     :: options -> state

class GameState2 g where
    getState2    :: (SafeCopy state) => g -> state
    newGame2     :: (Options options, SafeCopy state) => g -> options -> state -- takes blank g and applies options?

class (SafeCopy state, Command command) => GameCommand state command where
    runCommand  :: (HasAcidState m state) => command -> m state

class (GameState state options, GameCommand state command) => Game id state options command where
    getId       :: g -> id

-- is this useful? does game have enough of an idea of how it is stored to implement this?
-- startGame :: (GameState state command, HasAcidState m state, Monad m) => options -> m () 

