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

data UserLocation game = UserLocation
    { _user     :: UserId
    , _location :: Maybe game
    }

$(makeLens ''UserLocation)

deriving instance (Ord game) => Ord (UserLocation game)
deriving instance (Eq game) => Eq (UserLocation game)
deriving instance (Read game) => Read (UserLocation game)
deriving instance (Show game) => Show (UserLocation game)
deriving instance (Data game) => Data (UserLocation game)
deriving instance Typeable1 UserLocation

instance (Ord game, Typeable game) => Indexable (UserLocation game) where
    empty = ixSet [ ixFun $ \location -> [ user ^$ location ]
                  , ixFun $ \location -> [ _location location ]
                  ]

instance (SafeCopy game) => SafeCopy (UserLocation game) where
    putCopy (UserLocation user game) = contain $ do safePut user; safePut game
    getCopy = contain $ UserLocation <$> safeGet <*> safeGet

data LocationState game = LocationState
    { _locations :: IxSet (UserLocation game)
    }

initialLocationState :: (Ord game, Typeable game) => LocationState game
initialLocationState = LocationState empty

$(makeLens ''LocationState)

instance (Ord game, Typeable game, SafeCopy game) => SafeCopy (LocationState game) where
     version = 0
     kind = base
     putCopy (LocationState locations) = contain $ safePut locations
     getCopy = contain $ LocationState <$> safeGet

getLocation :: (Ord game, Typeable game) => UserId -> Query (LocationState game) (Maybe game)
getLocation userId =
    do  locationState <- ask
        case getOne $ (locations ^$ locationState) @= userId of
            Nothing  -> return Nothing
            Just loc -> return $ location ^$ loc

setLocation :: (Ord game, Typeable game) => UserId -> Maybe game -> Update (LocationState game) ()
setLocation userId mLocation =
    do  locationState <- get
        case getOne $ (locations ^$ locationState) @= userId of
            Nothing  -> return ()
            Just loc -> do  locations %= updateIx userId ((location ^!= mLocation) loc)
                            return ()

data SetLocation game = SetLocation UserId (Maybe game)
data GetLocation game = GetLocation UserId

deriving instance Typeable1 SetLocation
instance (SafeCopy game) => SafeCopy (SetLocation game) where
    putCopy (SetLocation user location) = contain $ do safePut user; safePut location;
    getCopy = contain $ SetLocation <$> safeGet <*> safeGet
instance (SafeCopy game, Typeable game) => Method (SetLocation game) where
    type MethodResult (SetLocation game)= ()
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
    acidEvents = [ UpdateEvent (\(SetLocation userId location) -> setLocation userId location)
                 , QueryEvent (\(GetLocation userId)                -> getLocation userId)
                 ]
