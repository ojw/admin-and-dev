{-# LANGUAGE TemplateHaskell, DeriveDataTypeable #-}

module Core.Game.Acid.Types.Options where

import Data.SafeCopy
import Data.Lens.Template
import Data.Data

data Options = Options
    { _timePerTurn  :: Int
    } deriving (Ord, Eq, Read, Show, Data, Typeable)

$(makeLens ''Options)
$(deriveSafeCopy 0 'base ''Options)
