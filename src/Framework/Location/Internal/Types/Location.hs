{-# LANGUAGE GeneralizedNewtypeDeriving, DeriveDataTypeable, TemplateHaskell, 
    MultiParamTypeClasses, FlexibleContexts, FlexibleInstances #-}

module Framework.Location.Internal.Types.Location where

import Control.Monad.RWS
import Data.Functor.Identity
import Control.Monad hiding ( join )
import Data.Functor
import Control.Monad.State hiding ( join )
import Control.Monad.Reader hiding ( join )
import Control.Monad.Error hiding ( join )
import Data.SafeCopy
import Data.Data
import Data.Acid
import Data.Lens
import Data.Lens.Template
import Data.IxSet
import Data.Text                            ( Text )

import Framework.Profile                ( UserId, Profile, ProfileState, ProfileInfo )
import Framework.Location.Internal.Types.Lobby as Lobby
import Framework.Location.Internal.Types.Matchmaker as Matchmaker
import Framework.Location.Internal.Types.Matchmaker as Matchmaker
import Framework.Location.Internal.Types.Game as Game
import Framework.Location.Internal.Types.Chat hiding ( addChat )

data LocationId = InLobby LobbyId | InMatchmaker MatchmakerId | WatchingGame GameId | InGame GameId
    deriving (Ord, Eq, Read, Show, Data, Typeable)

deriveSafeCopy 0 'base ''LocationId

data UserLocation = UserLocation
    { _userId        :: UserId
    , _locationId    :: LocationId
    } deriving (Ord, Eq, Read, Show, Data, Typeable)

makeLens ''UserLocation
deriveSafeCopy 0 'base ''UserLocation
inferIxSet "UserLocations" ''UserLocation 'noCalcs [''UserId, ''LocationId]

data LocationState = LocationState
    { _userLocations    :: UserLocations
    , _defaultLobbyId   :: LobbyId
    , _lobbyState       :: LobbyState
    , _matchmakerState  :: MatchmakerState
    , _gameState        :: GameState
    } deriving (Ord, Eq, Read, Show, Data, Typeable)

makeLens ''LocationState

data LocationError = LocationDoesNotExist | OtherLocationError

instance Error LocationError where
    noMsg = OtherLocationError

--type LocationErrorMonad = Either LocationError

data Location
    = LocLobby Lobby
    | LocMatchmaker Matchmaker
    | LocGame Game
    deriving (Ord, Eq, Read, Show, Data, Typeable)

class (MonadReader (Profile, ProfileState) m, MonadError LocationError m, Functor m, MonadState LocationState m) => MonadLocationAction m

newtype LocationAction a = LocationAction { unLocationAction :: (RWST ProfileInfo Text LocationState (ErrorT LocationError Identity) a) }
    deriving (Monad, MonadError LocationError, Functor, MonadState LocationState, MonadReader ProfileInfo)

instance MonadLocationAction LocationAction


runLocationAction (LocationAction locationAction) profileInfo locationState = do
    runIdentity $ runErrorT $ (runRWST locationAction) profileInfo locationState

setLocation :: MonadState LocationState m => LocationId -> UserId -> m ()
setLocation locationId userId = do
    userLocations %= updateIx userId (UserLocation userId locationId)
    return ()

getUserLocation :: MonadState LocationState m => UserId -> m LocationId
getUserLocation userId = do
    userLocations <- gets _userLocations
    case _locationId <$> (getOne $ userLocations @= userId) of
        Nothing -> do 
            defaultLobbyId <- gets _defaultLobbyId
            return $ InLobby $ defaultLobbyId
        Just locationId -> return locationId

getUsers :: MonadState LocationState m => LocationId -> m [UserId]
getUsers locationId = do
    userLocations <- gets _userLocations
    return $ _userId <$> toList (userLocations @= locationId) 

inGame :: MonadState LocationState m => UserId -> m Bool
inGame userId = do
    userLocation <- getUserLocation userId
    case userLocation of
        InGame _    -> return True
        _           -> return False

modLobby :: (Functor m, MonadState LocationState m) => (Lobby -> Lobby) -> LobbyId -> m (Maybe Lobby)
modLobby f lobbyId = do
    lobbies <- _lobbies <$> gets _lobbyState
    return $ f <$> (getOne $ lobbies @= lobbyId)

getLobby :: (Functor m, MonadState LocationState m) => LobbyId -> m (Maybe Lobby)
getLobby = modLobby id

modMatchmaker :: (Functor m, MonadState LocationState m) => (Matchmaker -> Matchmaker) -> MatchmakerId -> m (Maybe Matchmaker)
modMatchmaker f matchmakerId = do
    matchmakers <- _matchmakers <$> gets _matchmakerState
    return $ f <$> (getOne $ matchmakers @= matchmakerId)

getMatchmaker :: (Functor m, MonadState LocationState m) => MatchmakerId -> m (Maybe Matchmaker)
getMatchmaker = modMatchmaker id

modGame :: (Functor m, MonadState LocationState m) => (Game -> Game) -> GameId -> m (Maybe Game)
modGame f gameId = do
    games <- _games <$> gets _gameState
    return $ f <$> (getOne $ games @= gameId)

getGame :: (Functor m, MonadState LocationState m) => GameId -> m (Maybe Game)
getGame = modGame id

getLocation :: (Functor m, MonadState LocationState m) => LocationId -> m (Maybe Location)
getLocation (InLobby lobbyId) = getLobby lobbyId >>= return . fmap LocLobby
getLocation (InMatchmaker matchmakerId) = getMatchmaker matchmakerId >>= return . fmap LocMatchmaker
getLocation (InGame gameId) = getGame gameId >>= return . fmap LocGame
getLocation (WatchingGame gameId) = getGame gameId >>= return . fmap LocGame
