{-# LANGUAGE GeneralizedNewtypeDeriving, DeriveDataTypeable, TemplateHaskell, 
    MultiParamTypeClasses, FlexibleContexts, FlexibleInstances, TypeFamilies,
    OverloadedStrings #-}

module Framework.Location.Internal.Types.Location where

import Control.Monad.RWS hiding ( modify )
import Data.Functor.Identity
import Control.Monad hiding ( join )
import Data.Functor
import Control.Monad.State hiding ( join, modify )
import Control.Monad.Reader hiding ( join )
import Control.Monad.Error hiding ( join )
import Data.SafeCopy
import Data.Data
import Data.Acid hiding ( query, update )
import Data.Lens
import Data.Lens.Template
import Data.IxSet hiding ( delete )
import Data.Text                            ( Text )

import Framework.Profile                ( UserId(..), Profile, ProfileState, ProfileInfo )
import Framework.Location.Internal.Types.Lobby as L
import Framework.Location.Internal.Types.Matchmaker as M
import Framework.Location.Internal.Types.Game as G
import Framework.Location.Internal.Types.Chat hiding ( addChat )
import Framework.Common.Classes as Classes ( IndexedContainer(..), Create(..) )

import Util.HasAcidState as Acid

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

instance IndexedContainer LobbyId Lobby LobbyState where
    add lobby (LobbyState lobbies nextLobbyId) = (nextLobbyId, (LobbyState lobbies' (succ nextLobbyId))) 
        where
            lobbies' = updateIx nextLobbyId (lobby { L._lobbyId = nextLobbyId }) lobbies
    modify lobbyId f (LobbyState lobbies n) = (mLobby, (LobbyState lobbies' n)) 
        where
            mLobby = f <$> (getOne $ lobbies @= lobbyId)
            lobbies' = case mLobby of
                        Just lobby -> updateIx lobbyId lobby lobbies
                        Nothing -> lobbies
    delete lobbyId (LobbyState lobbies n) = (LobbyState (deleteIx lobbyId lobbies) n)

data LobbyOptions = LobbyOptions
    { lobbyOptionsName          :: Maybe Text
    , lobbyOptionsDescription   :: Maybe Text
    }

instance Create LobbyOptions Lobby where
    blank = Lobby "" "" (LobbyId 0) []
    update options lobby = lobby 
        { _name = maybe (_name lobby) id (lobbyOptionsName options)
        , _description = maybe (_description lobby) id (lobbyOptionsDescription options)
        }

instance IndexedContainer MatchmakerId Matchmaker MatchmakerState where
    add matchmaker (MatchmakerState matchmakers nextMatchmakerId) = (nextMatchmakerId, (MatchmakerState matchmakers' (succ nextMatchmakerId))) 
        where
            matchmakers' = updateIx nextMatchmakerId (matchmaker { M._matchmakerId = nextMatchmakerId }) matchmakers
    modify matchmakerId f (MatchmakerState matchmakers n) = (mMatchmaker, (MatchmakerState matchmakers' n)) 
        where
            mMatchmaker = f <$> (getOne $ matchmakers @= matchmakerId)
            matchmakers' = case mMatchmaker of
                        Just matchmaker -> updateIx matchmakerId matchmaker matchmakers
                        Nothing -> matchmakers
    delete matchmakerId (MatchmakerState matchmakers n) = (MatchmakerState (deleteIx matchmakerId matchmakers) n)

data MatchmakerOptions = MatchmakerOptions
    { matchmakerOptionsCapacity :: Maybe (Int, Int)
    }

instance Create MatchmakerOptions Matchmaker where
    blank = Matchmaker (MatchmakerId 0) [] (2,2) (UserId 0) (LobbyId 0)
    update options matchmaker = matchmaker { _capacity = maybe (_capacity matchmaker) id (matchmakerOptionsCapacity options) }

instance IndexedContainer GameId Game GameState where
    add game (GameState games nextGameId) = (nextGameId, (GameState games' (succ nextGameId))) 
        where
            games' = updateIx nextGameId (game { _gameId = nextGameId }) games
    modify gameId f (GameState games n) = (mGame, (GameState games' n)) 
        where
            mGame = f <$> (getOne $ games @= gameId)
            games' = case mGame of
                        Just game -> updateIx gameId game games
                        Nothing -> games
    delete gameId (GameState games n) = (GameState (deleteIx gameId games) n)

data GameOptions = GameOptions

instance Create GameOptions Game where  
    blank = Game (GameId 0) (MatchmakerId 0) (LobbyId 0) []
    update options game = game

instance IndexedContainer LocationId Location LocationState where
    add (LocLobby lobby) locationState = (InLobby lobbyId, locationState')
        where
            lobbyState = _lobbyState locationState
            (lobbyId, lobbyState') = add lobby lobbyState
            locationState' = locationState { _lobbyState = lobbyState' }
    add (LocMatchmaker matchmaker) locationState = (InMatchmaker matchmakerId, locationState')
        where
            matchmakerState = _matchmakerState locationState
            (matchmakerId, matchmakerState') = add matchmaker matchmakerState
            locationState' = locationState { _matchmakerState = matchmakerState' }
    add (LocGame game) locationState = (InGame gameId, locationState')
        where
            gameState = _gameState locationState
            (gameId, gameState') = add game gameState
            locationState' = locationState { _gameState = gameState' }
    modify (InLobby lobbyId) f locationState = (LocLobby <$> mLobby', locationState')
        where
            lobbyState = _lobbyState locationState
            f' = \lobby -> unLocLobby (f (LocLobby lobby))
            (mLobby', lobbyState') = modify lobbyId f' lobbyState :: (Maybe Lobby, LobbyState)
            locationState' = locationState { _lobbyState = lobbyState' }
    modify (InMatchmaker matchmakerId) f locationState = (LocMatchmaker <$> mMatchmaker', locationState')
        where
            matchmakerState = _matchmakerState locationState
            f' = \matchmaker -> unLocMatchmaker (f (LocMatchmaker matchmaker))
            (mMatchmaker', matchmakerState') = modify matchmakerId f' matchmakerState :: (Maybe Matchmaker, MatchmakerState)
            locationState' = locationState { _matchmakerState = matchmakerState' }
    modify (InGame gameId) f locationState = (LocGame <$> mGame', locationState')
        where
            gameState = _gameState locationState
            f' = \game -> unLocGame (f (LocGame game))
            (mGame', gameState') = modify gameId f' gameState :: (Maybe Game, GameState)
            locationState' = locationState { _gameState = gameState' }
    delete (InLobby lobbyId) locationState = locationState'
        where
            lobbyState = _lobbyState locationState
            lobbyState' = delete lobbyId lobbyState
            locationState' = locationState { _lobbyState = lobbyState' }
    delete (InMatchmaker matchmakerId) locationState = locationState'
        where
            matchmakerState = _matchmakerState locationState
            matchmakerState' = delete matchmakerId matchmakerState
            locationState' = locationState { _matchmakerState = matchmakerState' }
    delete (InGame gameId) locationState = locationState'
        where
            gameState = _gameState locationState
            gameState' = delete gameId gameState
            locationState' = locationState { _gameState = gameState' }

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

data LocationOptions = LOLobby LobbyOptions | LOMatchmaker MatchmakerOptions | LOGame GameOptions

-- Create class doesn't work as well for Locations in general.
-- Blank has to chose which type to be, so I defaulted to Lobby.
-- Update only makes sense if the options and object types match up.
-- I think this is guaranteed by functional dependencies, but I added a pattern match that does nothing in case a mismatched pair is passed to update.
instance Create LocationOptions Location where
    blank = LocLobby (blank :: Lobby)
    update (LOLobby lobbyOptions) (LocLobby lobby) = LocLobby $ Classes.update lobbyOptions lobby
    update (LOMatchmaker matchmakerOptions) (LocMatchmaker matchmaker) = LocMatchmaker $ Classes.update matchmakerOptions matchmaker
    update (LOGame gameOptions) (LocGame game) = LocGame $ Classes.update gameOptions game
    update _ loc = loc

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
