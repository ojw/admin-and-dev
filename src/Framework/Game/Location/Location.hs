{-# LANGUAGE GeneralizedNewtypeDeriving, DeriveDataTypeable, TemplateHaskell, 
    MultiParamTypeClasses, FlexibleContexts, FlexibleInstances #-}

module Framework.Game.Location.Location where

import Control.Monad hiding ( join )
import Data.Functor
import Control.Monad.State hiding ( join )
import Control.Monad.Reader hiding ( join )
import Data.SafeCopy
import Data.Data
import Data.Acid
import Data.Lens
import Data.Lens.Template
import Data.IxSet
import Data.Text                            ( Text )

import Framework.Auth.Auth                  ( UserId )
import Framework.Profile.Profile as Profile
import Framework.Game.Location.Lobby as Lobby
import Framework.Game.Location.Matchmaker as Matchmaker
import Framework.Game.Location.Game as Game

data LocationId = InLobby LobbyId | InMatchmaker MatchmakerId | WatchingGame GameId | InGame GameId
    deriving (Ord, Eq, Read, Show, Data, Typeable)

$(deriveSafeCopy 0 'base ''LocationId)

data UserLocation = UserLocation
    { _userId        :: UserId
    , _locationId    :: LocationId
    } deriving (Ord, Eq, Read, Show, Data, Typeable)

$(makeLens ''UserLocation)
$(deriveSafeCopy 0 'base ''UserLocation)
$(inferIxSet "UserLocations" ''UserLocation 'noCalcs [''UserId, ''LocationId])

data LocationState = LocationState
    { _userLocations    :: UserLocations
    , _defaultLobbyId   :: LobbyId
    , _lobbyState       :: LobbyState
    , _matchmakerState  :: MatchmakerState
    , _gameState        :: GameState
    } deriving (Ord, Eq, Read, Show, Data, Typeable)

$(makeLens ''LocationState)

class (Profile p, Functor m, MonadState LocationState m, MonadReader p m) => LocationAction p m

setLocation :: MonadState LocationState m => LocationId -> UserId -> m ()
setLocation locationId userId = do
    userLocations %= updateIx userId (UserLocation userId locationId)
    return ()

getLocation :: MonadState LocationState m => UserId -> m LocationId
getLocation userId = do
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
    userLocation <- getLocation userId
    case userLocation of
        InGame _    -> return True
        _           -> return False

-- these are obviously totally wrong and temporary
class (LocationAction p m) => Location l p m where
    canJoin     :: l -> m Bool
    onJoin      :: l -> m ()
    canLeave    :: l -> m Bool
    onLeave     :: l -> m ()
    exit        :: l -> m LocationId

instance (LocationAction p m) => Location Lobby p m where
    canJoin _ = return True
    onJoin _ = return ()
    canLeave _ = return True
    onLeave _ = return ()
    exit = return . InLobby . Lobby._lobbyId

instance (LocationAction p m) => Location Matchmaker p m where
    canJoin matchmaker = do
        users <- getUsers $ InMatchmaker $ Matchmaker._matchmakerId matchmaker
        return $ length users < snd (_capacity matchmaker)
    onJoin _ = return ()
    canLeave _ = return True
    onLeave matchmaker = do
        userId <- asks Profile.userId
        if userId == _owner matchmaker
        then do
            users <- getUsers $ InMatchmaker $ Matchmaker._matchmakerId matchmaker
            exit <- exit matchmaker            
            mapM_ (setLocation exit) users
        else return ()
    exit = return . InLobby . Matchmaker._lobbyId

instance (LocationAction p m) => Location Game p m where
    canJoin _ = return True
    onJoin _ = return ()
    canLeave _ = return True
    onLeave _ = return ()
    exit = return . InLobby . Game._lobbyId

-- removed UserId since these will run with MonadReader Profile m
data LocationApi
    = Join LocationId
    | Leave 
    | Look LocationId -- will include data previously requested with ReceiveChat
    | Chat Text LocationId
    | Create LocationId -- will probably patern match on LocationId to determine type of location, ignore id
    | Delete LocationId

instance (LocationAction p m) => Location LocationId p m where
    canJoin locationId = return True
    onJoin locationId = return ()
    canLeave locationId = return True
    onLeave locationId = return ()
    exit (InLobby lobbyId) = do
        lobbies <- _lobbies <$> gets _lobbyState
        case getOne $ lobbies @= lobbyId of
            Nothing -> InLobby <$> gets _defaultLobbyId
            Just lobby -> exit lobby
    exit (InMatchmaker matchmakerId) = do
        matchmakers <- _matchmakers <$> gets _matchmakerState
        case getOne $ matchmakers @= matchmakerId of
            Nothing -> InLobby <$> gets _defaultLobbyId
            Just lobby -> exit lobby
    exit (InGame gameId) = do
        games <- _games <$> gets _gameState
        case getOne $ games @= gameId of
            Nothing -> InLobby <$> gets _defaultLobbyId
            Just game -> exit game
    exit (WatchingGame gameId) = do
        games <- _games <$> gets _gameState
        case getOne $ games @= gameId of
            Nothing -> InLobby <$> gets _defaultLobbyId
            Just game -> exit game

join :: (LocationAction p m) => LocationId -> m ()
join locationId = do
    userId <- asks Profile.userId
    oldLocationId <- getLocation userId
    canLeave <- canLeave locationId
    canJoin <- canJoin locationId
    if canJoin && canLeave then setLocation locationId userId >> onLeave oldLocationId >> onJoin locationId else return () 

leave :: (LocationAction p m) => m ()
leave = do
    userId <- asks Profile.userId
    locationId <- getLocation userId
    join locationId
