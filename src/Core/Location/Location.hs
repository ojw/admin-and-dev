{-# LANGUAGE TemplateHaskell, Rank2Types, StandaloneDeriving, DeriveDataTypeable, TypeFamilies #-}

module Location where

import Control.Monad.State  ( get )
import Control.Monad.Reader ( ask )
import Control.Applicative
import Data.Functor
import Data.IxSet
import Data.Acid
import Data.Data
import Data.Lens
import Data.Lens.Template
import Data.SafeCopy

import Core.Auth.Auth       ( UserId )

data Location game = Location
    { _user          :: UserId
    , _game          :: game
    }

$(makeLens ''Location)

deriving instance (Ord game) => Ord (Location game)
deriving instance (Eq game) => Eq (Location game)
deriving instance (Read game) => Read (Location game)
deriving instance (Show game) => Show (Location game)
deriving instance (Data game) => Data (Location game)
deriving instance Typeable1 Location

instance (Ord game, Typeable game) => Indexable (Location game) where
    empty = ixSet [ ixFun $ \location -> [ user ^$ location ]
                  , ixFun $ \location -> [ game ^$ location ]
                  ]

instance (SafeCopy game) => SafeCopy (Location game) where
    putCopy (Location user game) = contain $ do safePut user; safePut game
    getCopy = contain $ Location <$> safeGet <*> safeGet

data LocationState game = LocationState
    { _locations :: IxSet (Location game)
    }

$(makeLens ''LocationState)

instance (Ord game, Typeable game, SafeCopy game) => SafeCopy (LocationState game) where
     version = 0
     kind = base
     putCopy (LocationState locations) = contain $ safePut locations
     getCopy = contain $ LocationState <$> safeGet

setLocation :: (Ord game, Typeable game) => UserId -> game -> Update (LocationState game) game
setLocation userId game = 
    do  locations %= updateIx userId (Location userId game)
        return game

getLocation :: (Ord game, Typeable game) => UserId -> Query (LocationState game) (Maybe game)
getLocation userId =
    do  locationState <- ask
        case getOne $ (locations ^$ locationState) @= userId of
            Nothing  -> return Nothing
            Just loc -> return $ Just $ game ^$ loc

$(makeAcidic ''LocationState ['setLocation, 'getLocation])
