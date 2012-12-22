{-# LANGUAGE DeriveDataTypeable, GADTs, TemplateHaskell, GeneralizedNewtypeDeriving,
    OverloadedStrings, StandaloneDeriving, TypeFamilies, ScopedTypeVariables, 
    MultiParamTypeClasses #-}

module Server.Location.Location where

import Control.Monad.State hiding ( join )
import Data.Functor

import Data.IxSet hiding ( delete )
import Data.Acid
import Data.SafeCopy
import Data.Data
import Data.Lens
import Data.Lens.Template
import Data.ByteString.Lazy.Char8 hiding ( map, length, filter )

import Server.Auth.Acid             ( UserId )
import Server.Location.Lobby as Lobby --( LobbyState(..), LobbyId )
import Server.Location.Matchmaker as Matchmaker  --( MatchmakerState, MatchmakerId )
import Server.Location.Game as Game        --( GameState, GameId )
import Server.Location.Chat

data LocationId = Nowhere | InLobby LobbyId | InMatchmaker MatchmakerId | InGame GameId
    deriving (Ord, Eq, Read, Show, Data, Typeable)

data NoLocation = NoLocation

$(deriveSafeCopy 0 'base ''LocationId)

data UserLocation = UserLocation
    { _userId       :: UserId
    , _locationId   :: LocationId
    } deriving (Ord, Eq, Read, Show, Data, Typeable)

$(makeLens ''UserLocation)
$(deriveSafeCopy 0 'base ''UserLocation)

instance Indexable UserLocation where
    empty = ixSet [ ixFun $ \location -> [ userId ^$ location ]
                  , ixFun $ \location -> [ _locationId location ]
                  ]

newtype UserLocations = UserLocations { _userLocations :: IxSet UserLocation }
    deriving (Ord, Eq, Read, Show, Data, Typeable, SafeCopy)

$(makeLens ''UserLocations)

data LocationState = LocationState
    { _locations         :: UserLocations
    , _lobbyState        :: LobbyState
    , _matchmakerState   :: MatchmakerState
    , _gameState         :: GameState
    } deriving (Ord, Eq, Read, Show, Data, Typeable)   

$(makeLens ''LocationState)
$(deriveSafeCopy 0 'base ''LocationState)

newtype LocationAction a = LocationAction { _unLocationAction :: State LocationState a } deriving (Functor, Monad, MonadState LocationState)

-- the internal API for getting / setting users' location data

getLocation' :: UserId -> UserLocations -> LocationId
getLocation' userId (UserLocations userLocations) =
    case fmap _locationId $ getOne $ userLocations @= userId of
        Nothing     -> Nowhere
        Just loc    -> loc

getLocation :: UserId -> LocationAction LocationId
getLocation userId = do
    locationState <- get
    LocationAction $ return $ getLocation' userId $ _locations locationState

setLocation' :: UserId -> LocationId -> UserLocations -> UserLocations
setLocation' userId locationId (UserLocations userLocations) =
    UserLocations $ updateIx userId (UserLocation userId locationId) userLocations

setLocation :: UserId -> LocationId -> LocationAction ()
setLocation userId locationId = do
    locationState <- get
    locations %= setLocation' userId locationId
    LocationAction $ return ()

getMembers' :: LocationId -> UserLocations -> [UserId]
getMembers' locationId (UserLocations userLocations) =
    map _userId $ toList $ userLocations @= locationId

getMembers :: LocationId -> LocationAction [UserId]
getMembers locationId = do
    locationState <- get
    LocationAction $ return $ getMembers' locationId (_locations locationState)

doNothing :: LocationAction ()
doNothing = LocationAction $ return ()

returnLocationAction :: a -> LocationAction a
returnLocationAction = LocationAction . return

lookupLobby :: LobbyId -> LocationAction (Maybe Lobby)
lookupLobby lobbyId = do
    lobbyState <- gets _lobbyState
    LocationAction $ return $ getOne $ (_lobbies lobbyState) @= lobbyId

lookupMatchmaker :: MatchmakerId -> LocationAction (Maybe Matchmaker)
lookupMatchmaker matchmakerId = do
    matchmakerState <- gets _matchmakerState
    LocationAction $ return $ getOne $ (_matchmakers matchmakerState) @= matchmakerId

lookupGame :: GameId -> LocationAction (Maybe Game)
lookupGame gameId = do
    gameState <- gets _gameState
    LocationAction $ return $ getOne $ (_games gameState) @= gameId

-- internal API for manipulating individual locations

class Location location where
    join    :: UserId -> location -> LocationAction Bool -- whether join is allowed
    leave   :: UserId -> location -> LocationAction Bool -- whether leave is allowed
    look    :: location -> LocationAction ByteString
    blank   :: LocationAction location
    delete  :: location -> LocationAction Bool -- whether or not to actually delete location
    exit    :: location -> LocationAction LocationId

instance ChatRoom NoLocation where
    addChat chat NoLocation = NoLocation
    getChats NoLocation = []

instance Location NoLocation where
    join userId NoLocation = do
        returnLocationAction True
    leave userId NoLocation = returnLocationAction True
    look NoLocation = returnLocationAction "Quux"
    blank = returnLocationAction NoLocation
    delete NoLocation = returnLocationAction False
    exit _ = returnLocationAction Nowhere

instance Location Lobby where
    join userId lobby = do
        returnLocationAction True
    leave userId lobby = do
        returnLocationAction True
    look lobby = returnLocationAction "FOO"
    blank = returnLocationAction $ emptyLobby "New Lobby"
    delete lobby = returnLocationAction True
    exit _ = returnLocationAction Nowhere

instance Location Matchmaker where
    join userId matchmaker = do
        let newLocation = InMatchmaker (Matchmaker._matchmakerId matchmaker)
        members <- getMembers newLocation
        if length members >= (_capacity matchmaker)
            then returnLocationAction False
            else returnLocationAction True
    leave userId matchmaker = do
        let currentLocation = InMatchmaker (Matchmaker._matchmakerId matchmaker)
            owner = Matchmaker._owner matchmaker
        mExitLobby <- lookupLobby (Matchmaker._lobbyId matchmaker)
        case mExitLobby of
            Nothing -> do 
                returnLocationAction True -- something has gone wrong, this is reasonable fallback
            Just exitLobby ->
                if userId /= owner
                    then do
                        returnLocationAction True
                    else do
                        members <- getMembers currentLocation
                        mapM_ (\uid -> leave uid matchmaker) $ filter (/= userId) members
                        returnLocationAction True
    look matchmaker = returnLocationAction "BAR"
    blank = returnLocationAction stupidEmptyMatchmaker
    delete matchmaker = do
        mExitLobby <- lookupLobby (Matchmaker._lobbyId matchmaker)
        members <- getMembers (InMatchmaker (Matchmaker._matchmakerId matchmaker))
        case mExitLobby of
            Nothing -> mapM_ (\uid -> setLocation uid Nowhere) members
            Just exitLobby -> mapM_ (\uid -> join uid exitLobby) members
        returnLocationAction True
    exit = returnLocationAction . InLobby . Matchmaker._lobbyId

instance Location Game where
    join userId game = do -- joining isn't the same as starting, doesn't mean you're PLAYING the game
        returnLocationAction True
    leave userId game = do
        -- only a spectator can just "leave"
        -- a player has to "quit"
        -- this needs to look somewhere outside the scope of LocationAction
        -- so revision is needed here
        returnLocationAction True
    look game = returnLocationAction "BLA!"
    blank = returnLocationAction stupidEmptyGame
    delete game = do
        mExitLobby <- lookupLobby (Game._lobbyId game)
        members <- getMembers (InMatchmaker (Game._matchmakerId game))
        case mExitLobby of
            Nothing -> mapM_ (\uid -> setLocation uid Nowhere) members
            Just exitLobby -> mapM_ (\uid -> join uid exitLobby) members
        returnLocationAction True
    exit = returnLocationAction . InLobby . Game._lobbyId

instance Location LocationId where
    join userId locationId =
        case locationId of
            Nowhere -> join userId NoLocation
            InLobby lobbyId -> do
                mLobby <- lookupLobby lobbyId
                case mLobby of
                    Nothing -> returnLocationAction False
                    Just lobby -> join userId lobby
            InMatchmaker matchmakerId -> do
                mMatchmaker <- lookupMatchmaker matchmakerId
                case mMatchmaker of
                    Nothing -> returnLocationAction False
                    Just matchmaker -> join userId matchmaker
            InGame gameId -> do
                mGame <- lookupGame gameId
                case mGame of
                    Nothing -> returnLocationAction False
                    Just game -> join userId game
    leave userId locationId =
        case locationId of
            Nowhere -> leave userId NoLocation
            InLobby lobbyId -> do
                mLobby <- lookupLobby lobbyId
                case mLobby of
                    Nothing -> returnLocationAction False
                    Just lobby -> leave userId lobby
            InMatchmaker matchmakerId -> do
                mMatchmaker <- lookupMatchmaker matchmakerId
                case mMatchmaker of
                    Nothing -> returnLocationAction False
                    Just matchmaker -> leave userId matchmaker
            InGame gameId -> do
                mGame <- lookupGame gameId
                case mGame of
                    Nothing -> returnLocationAction False
                    Just game -> leave userId game
    look locationId =
        case locationId of
            Nowhere -> look NoLocation
            InLobby lobbyId -> do
                mLobby <- lookupLobby lobbyId
                case mLobby of
                    Nothing -> returnLocationAction "Nothing."
                    Just lobby -> look lobby
            InMatchmaker matchmakerId -> do
                mMatchmaker <- lookupMatchmaker matchmakerId
                case mMatchmaker of
                    Nothing -> returnLocationAction "Nothing."
                    Just matchmaker -> look matchmaker
            InGame gameId -> do
                mGame <- lookupGame gameId
                case mGame of
                    Nothing -> returnLocationAction "Nothing."
                    Just game -> look game
    blank = returnLocationAction Nowhere
    delete locationId =
        case locationId of
            Nowhere -> delete NoLocation
            InLobby lobbyId -> do
                mLobby <- lookupLobby lobbyId
                case mLobby of
                    Nothing -> returnLocationAction False
                    Just lobby -> delete lobby
            InMatchmaker matchmakerId -> do
                mMatchmaker <- lookupMatchmaker matchmakerId
                case mMatchmaker of
                    Nothing -> returnLocationAction False
                    Just matchmaker -> delete matchmaker
            InGame gameId -> do
                mGame <- lookupGame gameId
                case mGame of
                    Nothing -> returnLocationAction False
                    Just game -> delete game
    exit locationId =
        case locationId of
            Nowhere -> exit NoLocation
            InLobby lobbyId -> do
                mLobby <- lookupLobby lobbyId
                case mLobby of
                    Nothing -> returnLocationAction Nowhere
                    Just lobby -> exit lobby
            InMatchmaker matchmakerId -> do
                mMatchmaker <- lookupMatchmaker matchmakerId
                case mMatchmaker of
                    Nothing -> returnLocationAction Nowhere
                    Just matchmaker -> exit matchmaker
            InGame gameId -> do
                mGame <- lookupGame gameId
                case mGame of
                    Nothing -> returnLocationAction Nowhere
                    Just game -> exit game
    

data LocationApi
    = Join UserId LocationId
    | Leave UserId LocationId
    | Create UserId LocationId
    | Delete UserId LocationId
    | Look UserId LocationId
    | Send UserId Chat
    | Receive UserId 

runLocationApi :: LocationApi -> LocationAction ()
runLocationApi api = 
    case api of
        Join userId locationId -> join userId locationId >> doNothing
        Leave userId locationId -> leave userId locationId >> doNothing
        Look userId locationId -> look locationId >> doNothing
