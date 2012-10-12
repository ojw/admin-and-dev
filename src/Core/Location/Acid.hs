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
import Core.Lobby.Acid      ( LobbyId )

data SubLocation = InLobby LobbyId | InMatchmaker | InGame
    deriving (Ord, Eq, Read, Show, Data, Typeable)

$(deriveSafeCopy 0 'base ''SubLocation)

data Location game = Location
    { _user     :: UserId
    , _game     :: game
    , _location :: SubLocation
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
                  , ixFun $ \location -> [ _location location ]
                  ]

instance (SafeCopy game) => SafeCopy (Location game) where
    putCopy (Location user game location) = contain $ do safePut user; safePut game; safePut location;
    getCopy = contain $ Location <$> safeGet <*> safeGet <*> safeGet

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

setLocation :: (Ord game, Typeable game) => UserId -> game -> SubLocation -> Update (LocationState game) game
setLocation userId game subLocation = 
    do  locations %= updateIx userId (Location userId game subLocation)
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

data SetLocation game = SetLocation UserId game SubLocation
data GetLocation game = GetLocation UserId

deriving instance Typeable1 SetLocation
instance (SafeCopy game) => SafeCopy (SetLocation game) where
    putCopy (SetLocation user game location) = contain $ do safePut user; safePut game; safePut location;
    getCopy = contain $ SetLocation <$> safeGet <*> safeGet <*> safeGet
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
    acidEvents = [ UpdateEvent (\(SetLocation userId game location) -> setLocation userId game location)
                 , QueryEvent (\(GetLocation userId)       -> getLocation userId)
                 ]
