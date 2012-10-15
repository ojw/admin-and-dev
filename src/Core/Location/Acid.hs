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
import Core.Matchmaker.Acid ( MatchmakerId )
import Core.Game.Acid       ( GameId )

data Location = InLobby LobbyId | InMatchmaker MatchmakerId | InGame GameId
    deriving (Ord, Eq, Read, Show, Data, Typeable)

$(deriveSafeCopy 0 'base ''Location)

data UserLocation game = UserLocation
    { _user     :: UserId
    , _game     :: Maybe game
    , _location :: Maybe Location
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
                  , ixFun $ \location -> [ game ^$ location ]
                  , ixFun $ \location -> [ _location location ]
                  ]

instance (SafeCopy game) => SafeCopy (UserLocation game) where
    putCopy (UserLocation user game location) = contain $ do safePut user; safePut game; safePut location;
    getCopy = contain $ UserLocation <$> safeGet <*> safeGet <*> safeGet

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

setGameAndLocation :: (Ord game, Typeable game) => UserId -> Maybe game -> Maybe Location -> Update (LocationState game) ()
setGameAndLocation userId game location = 
    do  locations %= updateIx userId (UserLocation userId game location)
        return ()

getLocation :: (Ord game, Typeable game) => UserId -> Query (LocationState game) (Maybe Location)
getLocation userId =
    do  locationState <- ask
        case getOne $ (locations ^$ locationState) @= userId of
            Nothing  -> return Nothing
            Just loc -> return $ location ^$ loc

getGame :: (Ord game, Typeable game) => UserId -> Query (LocationState game) (Maybe game)
getGame userId =
    do  locationState <- ask
        case getOne $ (locations ^$ locationState) @= userId of
            Nothing  -> return Nothing
            Just loc -> return $ game ^$ loc

setGame :: (Ord game, Typeable game) => UserId -> Maybe game -> Update (LocationState game) ()
setGame userId mGame =
    do  locationState <- get
        case getOne $ (locations ^$ locationState) @= userId of
            Nothing  -> return ()
            Just loc -> do  locations %= updateIx userId ((game ^!= mGame) loc)
                            return ()

setLocation :: (Ord game, Typeable game) => UserId -> Maybe Location -> Update (LocationState game) ()
setLocation userId mLocation =
    do  locationState <- get
        case getOne $ (locations ^$ locationState) @= userId of
            Nothing  -> return ()
            Just loc -> do  locations %= updateIx userId ((location ^!= mLocation) loc)
                            return ()

-- below code is equivalent to just using
-- $(makeAcidic ''LocationState ['setGameAndLocation, 'getUserLocation])
-- but it avoids the duplicate constraint compilers warnings I get from the TH version

data SetGameAndLocation game = SetGameAndLocation UserId (Maybe game) (Maybe Location)
data GetLocation game = GetLocation UserId
data SetLocation game = SetLocation UserId (Maybe Location)
data GetGame game = GetGame UserId
data SetGame game = SetGame UserId (Maybe game)

deriving instance Typeable1 SetGameAndLocation
instance (SafeCopy game) => SafeCopy (SetGameAndLocation game) where
    putCopy (SetGameAndLocation user game location) = contain $ do safePut user; safePut game; safePut location;
    getCopy = contain $ SetGameAndLocation <$> safeGet <*> safeGet <*> safeGet
instance (SafeCopy game, Typeable game) => Method (SetGameAndLocation game) where
    type MethodResult (SetGameAndLocation game)= ()
    type MethodState (SetGameAndLocation game) = LocationState game
instance (SafeCopy game, Typeable game) => UpdateEvent (SetGameAndLocation game)

deriving instance Typeable1 SetLocation
instance (SafeCopy game) => SafeCopy (SetLocation game) where
    putCopy (SetLocation user location) = contain $ do safePut user; safePut location;
    getCopy = contain $ SetLocation <$> safeGet <*> safeGet
instance (SafeCopy game, Typeable game) => Method (SetLocation game) where
    type MethodResult (SetLocation game)= ()
    type MethodState (SetLocation game) = LocationState game
instance (SafeCopy game, Typeable game) => UpdateEvent (SetLocation game)

deriving instance Typeable1 SetGame
instance (SafeCopy game) => SafeCopy (SetGame game) where
    putCopy (SetGame user game) = contain $ do safePut user; safePut game;
    getCopy = contain $ SetGame <$> safeGet <*> safeGet
instance (SafeCopy game, Typeable game) => Method (SetGame game) where
    type MethodResult (SetGame game)= ()
    type MethodState (SetGame game) = LocationState game
instance (SafeCopy game, Typeable game) => UpdateEvent (SetGame game)

deriving instance Typeable1 GetLocation
instance (SafeCopy game) => SafeCopy (GetLocation game) where
    putCopy (GetLocation user) = contain $ safePut user
    getCopy = contain $ GetLocation <$> safeGet
instance (SafeCopy game, Typeable game) => Method (GetLocation game) where
    type MethodResult (GetLocation game) = Maybe Location
    type MethodState (GetLocation game) = LocationState game
instance (SafeCopy game, Typeable game) => QueryEvent (GetLocation game)

deriving instance Typeable1 GetGame
instance (SafeCopy game) => SafeCopy (GetGame game) where
    putCopy (GetGame user) = contain $ safePut user
    getCopy = contain $ GetGame <$> safeGet
instance (SafeCopy game, Typeable game) => Method (GetGame game) where
    type MethodResult (GetGame game) = Maybe game
    type MethodState (GetGame game) = LocationState game
instance (SafeCopy game, Typeable game) => QueryEvent (GetGame game)

instance (SafeCopy game, Ord game, Typeable game) => IsAcidic (LocationState game) where
    acidEvents = [ UpdateEvent (\(SetGameAndLocation userId game location) -> setGameAndLocation userId game location)
                 , UpdateEvent (\(SetLocation userId location) -> setLocation userId location)
                 , UpdateEvent (\(SetGame userId game) -> setGame userId game)
                 , QueryEvent (\(GetLocation userId)                -> getLocation userId)
                 , QueryEvent (\(GetGame userId)                    -> getGame userId)
                 ]
