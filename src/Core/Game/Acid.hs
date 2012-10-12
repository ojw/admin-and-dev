{-# LANGUAGE DeriveDataTypeable, GeneralizedNewtypeDeriving #-}

module Core.Game.Acid where

import Data.Data
import Data.SafeCopy

newtype GameId = GameId { gameId :: Int } deriving (Ord, Eq, Read, Show, Data, Typeable, SafeCopy)
