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
import Data.ByteString.Lazy.Char8 hiding ( map, length )

import Server.Auth.Acid             ( UserId )
import Server.Location.Lobby        --( LobbyState(..), LobbyId )
import Server.Location.Matchmaker   --( MatchmakerState, MatchmakerId )
import Server.Location.Game         --( GameState, GameId )
import Server.Location.Chat         ( ChatRoom )

data LocationId = Nowhere | InLobby LobbyId | InMatchmaker MatchmakerId | InGame GameId
    deriving (Ord, Eq, Read, Show, Data, Typeable)

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

-- add lookup function that turns Nothing into Nowhere, Just loc to loc

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

class (ChatRoom location) => Location location where
    join    :: UserId -> location -> LocationAction ()
    leave   :: UserId -> location -> LocationAction ()
    look    :: location -> String
    blank   :: location
    delete  :: location -> LocationAction Bool

instance Location Lobby where
    join userId lobby = setLocation userId $ InLobby $ Server.Location.Lobby._lobbyId lobby
    leave userId lobby = setLocation userId Nowhere
    look lobby = "FOO"
    blank = emptyLobby "New Lobby"
    delete lobby = returnLocationAction True

instance Location Matchmaker where
    join userId matchmaker = do
        let newLocation = InMatchmaker (_matchmakerId matchmaker)
        members <- getMembers newLocation
        if length members >= (_capacity matchmaker)
            then doNothing
            else setLocation userId newLocation
    leave userId matchmaker = do
        let currentLocation = InMatchmaker (_matchmakerId matchmaker)
            owner = _owner matchmaker
        mExitLobby <- lookupLobby (Server.Location.Matchmaker._lobbyId matchmaker)
        case mExitLobby of
            Nothing -> setLocation userId Nowhere -- something has gone wrong, this is reasonable fallback
            Just exitLobby ->
                if userId /= owner
                    then do
                        join userId exitLobby
                    else do
                        members <- getMembers currentLocation
                        mapM_ (\uid -> join uid exitLobby) members
    look matchmaker = "BAR"
    blank = stupidEmptyMatchmaker
    delete matchmaker = do
        mExitLobby <- lookupLobby (Server.Location.Matchmaker._lobbyId matchmaker)
        members <- getMembers (InMatchmaker (_matchmakerId matchmaker))
        case mExitLobby of
            Nothing -> mapM_ (\uid -> setLocation uid Nowhere) members
            Just exitLobby -> mapM_ (\uid -> join uid exitLobby) members
        returnLocationAction True
