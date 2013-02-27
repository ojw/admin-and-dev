{-# LANGUAGE GeneralizedNewtypeDeriving, DeriveDataTypeable, TemplateHaskell, 
    MultiParamTypeClasses, FlexibleContexts, FlexibleInstances, TypeFamilies #-}

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
import Data.Acid hiding ( query, update )
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

import Util.HasAcidState

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
deriveSafeCopy 0 'base ''LocationState


data LocationError = LocationDoesNotExist | OtherLocationError

instance Error LocationError where
    noMsg = OtherLocationError

data Location
    = LocLobby { unLocLobby :: Lobby }
    | LocMatchmaker { unLocMatchmaker :: Matchmaker }
    | LocGame { unLocGame :: Game }
    deriving (Ord, Eq, Read, Show, Data, Typeable)

deriveSafeCopy 0 'base ''Location

newtype LocationAction a = LocationAction { unLocationAction :: (RWST ProfileInfo Text (AcidState LocationState) (ErrorT LocationError IO) a) }
    deriving (Monad, MonadError LocationError, Functor, MonadState (AcidState LocationState), MonadReader ProfileInfo, MonadIO)

instance HasAcidState LocationAction LocationState where
    getAcidState = get

runLocationAction
    :: LocationAction a
    -> ProfileInfo
    -> AcidState LocationState
    -> IO (Either LocationError (a, AcidState LocationState, Text))
runLocationAction (LocationAction locationAction) profileInfo locationState = do
    runErrorT $ (runRWST locationAction) profileInfo locationState

setLocation' :: LocationId -> UserId -> Update LocationState ()
setLocation' locationId userId = do
    userLocations %= updateIx userId (UserLocation userId locationId)
    return ()

getUserLocation' :: UserId -> Query LocationState LocationId
getUserLocation' userId = do
    userLocations <- asks _userLocations
    case _locationId <$> (getOne $ userLocations @= userId) of
        Nothing -> do 
            defaultLobbyId <- asks _defaultLobbyId
            return $ InLobby $ defaultLobbyId
        Just locationId -> return locationId

getUsers' :: LocationId -> Query LocationState [UserId]
getUsers' locationId = do
    userLocations <- asks _userLocations
    return $ _userId <$> toList (userLocations @= locationId) 

getLobbies' :: Query LocationState Lobbies
getLobbies' = _lobbies <$> asks _lobbyState

getMatchmakers' :: Query LocationState Matchmakers
getMatchmakers' = _matchmakers <$> asks _matchmakerState

getGames' :: Query LocationState Games
getGames' = _games <$> asks _gameState

updateLobby' :: LobbyId -> Lobby -> Update LocationState ()
updateLobby' lobbyId lobby = do
    lobbyState %= (lobbies ^%= updateIx lobbyId lobby)
    return ()

updateMatchmaker' :: MatchmakerId -> Matchmaker -> Update LocationState ()
updateMatchmaker' matchmakerId matchmaker = do
    matchmakerState %= (matchmakers ^%= updateIx matchmakerId matchmaker)
    return ()

updateGame' :: GameId -> Game -> Update LocationState ()
updateGame' gameId game = do
    gameState %= (games ^%= updateIx gameId game)
    return ()

getDefaultLobbyId' :: Query LocationState LobbyId
getDefaultLobbyId' = asks _defaultLobbyId

makeAcidic ''LocationState ['setLocation', 'getUserLocation', 'getUsers', 'getLobbies', 'getMatchmakers', 'getGames', 'updateLobby', 'updateMatchmaker', 'updateGame', 'getDefaultLobbyId']

{-
-- Wrappers for the Acidic functions.
-- Might have separate newtype wrappers for pre/post acidic functions.
-}

setLocation :: LocationId -> UserId -> LocationAction ()
setLocation locationId userId = update $ SetLocation' locationId userId

getUserLocation :: UserId -> LocationAction LocationId
getUserLocation userId = query $ GetUserLocation' userId

getUsers :: LocationId -> LocationAction [UserId]
getUsers locationId = query $ GetUsers' locationId

inGame :: UserId -> LocationAction Bool
inGame userId = do
    userLocation <- getUserLocation userId
    case userLocation of
        InGame _    -> return True
        _           -> return False

getLobbies :: LocationAction Lobbies
getLobbies = query GetLobbies'

getLobby :: LobbyId -> LocationAction (Maybe Lobby)
getLobby lobbyId = do
    lobbies <- getLobbies
    return $ getOne $ lobbies @= lobbyId

getMatchmakers :: LocationAction Matchmakers
getMatchmakers = query GetMatchmakers'

getMatchmaker :: MatchmakerId -> LocationAction  (Maybe Matchmaker)
getMatchmaker matchmakerId = do
    matchmakers <- getMatchmakers
    return $ getOne $ matchmakers @= matchmakerId

getGames :: LocationAction Games
getGames = query GetGames'

getGame :: GameId -> LocationAction (Maybe Game)
getGame gameId = do
    games <- getGames
    return $ getOne $ games @= gameId

getLocation :: LocationId -> LocationAction (Maybe Location)
getLocation (InLobby lobbyId) = getLobby lobbyId >>= return . fmap LocLobby
getLocation (InMatchmaker matchmakerId) = getMatchmaker matchmakerId >>= return . fmap LocMatchmaker
getLocation (InGame gameId) = getGame gameId >>= return . fmap LocGame
getLocation (WatchingGame gameId) = getGame gameId >>= return . fmap LocGame

updateLobby :: LobbyId -> Lobby -> LocationAction ()
updateLobby lobbyId lobby = update $ UpdateLobby' lobbyId lobby

updateMatchmaker :: MatchmakerId -> Matchmaker -> LocationAction ()
updateMatchmaker matchmakerId matchmaker = update $ UpdateMatchmaker' matchmakerId matchmaker

updateGame :: GameId -> Game -> LocationAction ()
updateGame gameId game = update $ UpdateGame' gameId game

updateLocation :: LocationId -> Location -> LocationAction ()
updateLocation (InLobby lobbyId) (LocLobby lobby) = updateLobby lobbyId lobby
updateLocation (InMatchmaker matchmakerId) (LocMatchmaker matchmaker) = updateMatchmaker matchmakerId matchmaker
updateLocation (InGame gameId) (LocGame game) = updateGame gameId game
updateLocation (WatchingGame gameId) (LocGame game) = updateGame gameId game

getDefaultLobbyId :: LocationAction LobbyId
getDefaultLobbyId = query GetDefaultLobbyId'
