{-# LANGUAGE MultiParamTypeClasses #-}

module Game where

import Data.Aeson

import Util.HasAcidState

class Game state command options where
    getState    :: (HasAcidState m state) => m state
    runCommand  :: (HasAcidState m state) => command -> m state
    startGame   :: options -> state

-- probably should have something like this
-- at the very least...
class FromJSON c => Command c
class ToJSON st => GameState st
class FromJSON o => Options o
-- maybe replace ToJSON to ToResponse and usually implement via JSON
