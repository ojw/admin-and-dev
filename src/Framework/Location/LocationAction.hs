{-# LANGUAGE TemplateHaskell, MultiParamTypeClasses, GeneralizedNewtypeDeriving, TypeFamilies,
    DeriveDataTypeable #-}

module Framework.Location.LocationAction where

import Control.Monad.Error
import Control.Monad.RWS
import Data.Text
import Data.Acid hiding ( query, update )
import Data.IxSet hiding ( delete )
import Control.Lens as Lens
import Data.Functor

import Framework.Profile
import Util.HasAcidState as Acid
import Framework.Location.Types
import Framework.Location.Instances.IndexedContainer
import Framework.Location.Instances.Create
import Framework.Common.Classes

data LocationError = LocationDoesNotExist | OtherLocationError

instance Error LocationError where
    noMsg = OtherLocationError

newtype LocationAction a = LocationAction { unLocationAction :: (RWST ProfileInfo Text (AcidState LocationState) (ErrorT LocationError IO) a) }
    deriving (Monad, MonadError LocationError, Functor, MonadState (AcidState LocationState), MonadReader ProfileInfo, MonadIO)

instance HasAcidState LocationAction LocationState where
    getAcidState = get


add' :: Location -> Update LocationState LocationId
add' location = do
    locationState <- get
    let (locationId, locationState') = add location locationState
    put locationState
    return locationId

delete' :: LocationId -> Update LocationState ()
delete' locationId = do
    locationState <- get
    put $ delete locationId locationState

--data LocationOptions = LOLobby LobbyOptions | LOMatchmaker MatchmakerOptions | LOGame GameOptions

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
    userLocations <- Lens.view userLocations
    case fmap (Lens.view locationId) (getOne $ userLocations @= userId) of
        Nothing -> do 
            defaultLobbyId <- fmap (Lens.view defaultLobbyId) ask
            return $ InLobby $ defaultLobbyId
        Just locationId -> return locationId

getUsers' :: LocationId -> Query LocationState [UserId]
getUsers' locationId = do
    ul <- Lens.view userLocations -- fmap (Lens.view userLocations) ask
    return $ Lens.view Framework.Location.Types.userId <$> toList (ul @= locationId) 

getLobbies' :: Query LocationState Lobbies
getLobbies' = Lens.view (lobbyState . lobbies)

getMatchmakers' :: Query LocationState Matchmakers
getMatchmakers' = Lens.view (matchmakerState . matchmakers)

getGames' :: Query LocationState Games
getGames' = Lens.view (gameState . games)

updateLobby' :: LobbyId -> Lobby -> Update LocationState ()
updateLobby' lobbyId lobby = do
    lobbyState %= (lobbies %~ updateIx lobbyId lobby)
    return ()

updateMatchmaker' :: MatchmakerId -> Matchmaker -> Update LocationState ()
updateMatchmaker' matchmakerId matchmaker = do
    matchmakerState %= (matchmakers %~ updateIx matchmakerId matchmaker)
    return ()

updateGame' :: GameId -> Game -> Update LocationState ()
updateGame' gameId game = do
    gameState %= (games %~ updateIx gameId game)
    return ()

getDefaultLobbyId' :: Query LocationState LobbyId
getDefaultLobbyId' = fmap (Lens.view defaultLobbyId) ask

makeAcidic ''LocationState ['setLocation', 'getUserLocation', 'getUsers', 'getLobbies', 'getMatchmakers', 'getGames', 'updateLobby', 'updateMatchmaker', 'updateGame', 'getDefaultLobbyId', 'add', 'delete']

{-
-- Wrappers for the Acidic functions.
-- Might have separate newtype wrappers for pre/post acidic functions.
-}

setLocation :: LocationId -> UserId -> LocationAction ()
setLocation locationId userId = Acid.update $ SetLocation' locationId userId

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

add :: Location -> LocationAction LocationId
add location = Acid.update $ Add' location

delete locationId = Acid.update $ Delete' locationId

getLocation :: LocationId -> LocationAction (Maybe Location)
getLocation (InLobby lobbyId) = getLobby lobbyId >>= return . fmap LocLobby
getLocation (InMatchmaker matchmakerId) = getMatchmaker matchmakerId >>= return . fmap LocMatchmaker
getLocation (InGame gameId) = getGame gameId >>= return . fmap LocGame
getLocation (WatchingGame gameId) = getGame gameId >>= return . fmap LocGame

updateLobby :: LobbyId -> Lobby -> LocationAction ()
updateLobby lobbyId lobby = Acid.update $ UpdateLobby' lobbyId lobby

updateMatchmaker :: MatchmakerId -> Matchmaker -> LocationAction ()
updateMatchmaker matchmakerId matchmaker = Acid.update $ UpdateMatchmaker' matchmakerId matchmaker

updateGame :: GameId -> Game -> LocationAction ()
updateGame gameId game = Acid.update $ UpdateGame' gameId game

updateLocation :: LocationId -> Location -> LocationAction ()
updateLocation (InLobby lobbyId) (LocLobby lobby) = updateLobby lobbyId lobby
updateLocation (InMatchmaker matchmakerId) (LocMatchmaker matchmaker) = updateMatchmaker matchmakerId matchmaker
updateLocation (InGame gameId) (LocGame game) = updateGame gameId game
updateLocation (WatchingGame gameId) (LocGame game) = updateGame gameId game

getDefaultLobbyId :: LocationAction LobbyId
getDefaultLobbyId = query GetDefaultLobbyId'

