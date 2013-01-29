{-# LANGUAGE GeneralizedNewtypeDeriving, DeriveDataTypeable, TemplateHaskell #-}

module Framework.Auth where

import Data.Lens.Template
import Data.SafeCopy
import Data.Data
import Data.Acid

newtype UserId = UserId { _userId :: Int } deriving (Ord, Eq, Read, Show, Data, Typeable, SafeCopy)

$(makeLens ''UserId) 
