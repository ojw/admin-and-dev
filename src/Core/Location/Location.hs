{-# LANGUAGE TemplateHaskell, Rank2Types, StandaloneDeriving, DeriveDataTypeable #-}

module Location where

import Control.Applicative
import Data.Functor
import Data.IxSet
import Data.Acid
import Data.Data
import Data.SafeCopy

import Core.Auth.Auth       ( UserId )

data Location game = Location
    { user  :: UserId
    , game  :: game
    }

deriving instance (Ord game) => Ord (Location game)
deriving instance (Eq game) => Eq (Location game)
deriving instance (Read game) => Read (Location game)
deriving instance (Show game) => Show (Location game)
deriving instance (Data game) => Data (Location game)
deriving instance Typeable1 Location -- I don't know what this is about at all

instance (Ord game, Typeable game) => Indexable (Location game) where
    empty = ixSet [ ixFun $ \location -> [ user location ]
                  , ixFun $ \location -> [ game location ]
                  ]

instance (SafeCopy game) => SafeCopy (Location game) where
    putCopy (Location user game) = contain $ do safePut user; safePut game
    getCopy = contain $ Location <$> safeGet <*> safeGet

data LocationState game = LocationState
    { locations :: IxSet (Location game)
    }

instance (Ord game, Typeable game, SafeCopy game) => SafeCopy (LocationState game) where
     version = 0
     kind = base
     putCopy (LocationState locations) = contain $ safePut locations
     getCopy = contain $ LocationState <$> safeGet
