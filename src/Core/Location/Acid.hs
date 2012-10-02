{-# LANGUAGE TemplateHaskell, Rank2Types, StandaloneDeriving, DeriveDataTypeable, TypeFamilies #-}

module Core.Location.Acid where

import Control.Monad.State  ( get )
import Control.Monad.Reader ( ask )
import Control.Applicative hiding ( empty )
import Data.Functor
import Data.IxSet
import Data.Acid
import Data.Acid.Advanced
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

initialLocationState :: (Ord game, Typeable game) => LocationState game
initialLocationState = LocationState empty

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

-- below code is equivalent to just using
-- $(makeAcidic ''LocationState ['setLocation, 'getLocation])
-- but it avoids the duplicate constraint compilers warnings I get from the TH version

data SetLocation game = SetLocation UserId game
data GetLocation game = GetLocation UserId

deriving instance Typeable1 SetLocation
instance (SafeCopy game) => SafeCopy (SetLocation game) where
    putCopy (SetLocation user game) = contain $ do safePut user; safePut game
    getCopy = contain $ SetLocation <$> safeGet <*> safeGet
instance (SafeCopy game, Typeable game) => Method (SetLocation game) where
    type MethodResult (SetLocation game)= game
    type MethodState (SetLocation game) = LocationState game
instance (SafeCopy game, Typeable game) => UpdateEvent (SetLocation game)

deriving instance Typeable1 GetLocation
instance (SafeCopy game) => SafeCopy (GetLocation game) where
    putCopy (GetLocation user) = contain $ safePut user
    getCopy = contain $ GetLocation <$> safeGet
instance (SafeCopy game, Typeable game) => Method (GetLocation game) where
    type MethodResult (GetLocation game) = Maybe game
    type MethodState (GetLocation game) = LocationState game
instance (SafeCopy game, Typeable game) => QueryEvent (GetLocation game)

instance (SafeCopy game, Ord game, Typeable game) => IsAcidic (LocationState game) where
    acidEvents = [ UpdateEvent (\(SetLocation userId game) -> setLocation userId game)
                 , QueryEvent (\(GetLocation userId)       -> getLocation userId)
                 ]
