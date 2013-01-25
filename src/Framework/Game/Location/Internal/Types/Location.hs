{-# LANGUAGE GeneralizedNewtypeDeriving, DeriveDataTypeable, TemplateHaskell, 
    MultiParamTypeClasses, FlexibleContexts, FlexibleInstances #-}

module Framework.Game.Location.Internal.Types.Location where

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

import Framework.Auth.Auth                  ( UserId )
import Framework.Profile.Profile as Profile
import Framework.Game.Location.Internal.Types.Lobby as Lobby
import Framework.Game.Location.Internal.Types.Matchmaker as Matchmaker
import Framework.Game.Location.Internal.Types.Matchmaker as Matchmaker
import Framework.Game.Location.Internal.Types.Game as Game
import Framework.Game.Location.Internal.Types.Chat hiding ( addChat )

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

data LocationError = LocationDoesNotExist

class (Profile p, MonadError LocationError m, Functor m, MonadState LocationState m, MonadReader p m) => LocationAction p m

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

modLobby :: (LocationAction p m) => (Lobby -> Lobby) -> LobbyId -> m (Maybe Lobby)
modLobby f lobbyId = do
    lobbies <- _lobbies <$> gets _lobbyState
    return $ f <$> (getOne $ lobbies @= lobbyId)

getLobby :: (LocationAction p m) => LobbyId -> m (Maybe Lobby)
getLobby = modLobby id

modMatchmaker :: (LocationAction p m) => (Matchmaker -> Matchmaker) -> MatchmakerId -> m (Maybe Matchmaker)
modMatchmaker f matchmakerId = do
    matchmakers <- _matchmakers <$> gets _matchmakerState
    return $ f <$> (getOne $ matchmakers @= matchmakerId)

getMatchmaker :: (LocationAction p m) => MatchmakerId -> m (Maybe Matchmaker)
getMatchmaker = modMatchmaker id

modGame :: (LocationAction p m) => (Game -> Game) -> GameId -> m (Maybe Game)
modGame f gameId = do
    games <- _games <$> gets _gameState
    return $ f <$> (getOne $ games @= gameId)

getGame :: (LocationAction p m) => GameId -> m (Maybe Game)
getGame = modGame id

class (LocationAction p m) => Location l p m where
    canJoin     :: l -> m Bool
    onJoin      :: l -> m ()
    canLeave    :: l -> m Bool
    onLeave     :: l -> m ()
    exit        :: l -> m LocationId
    chat        :: Chat -> l -> m ()
