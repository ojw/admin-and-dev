{-# LANGUAGE DeriveDataTypeable, GADTs, TemplateHaskell, GeneralizedNewtypeDeriving,
    OverloadedStrings, StandaloneDeriving, TypeFamilies, ScopedTypeVariables, 
    MultiParamTypeClasses #-}

module Server.Location.Location where

import Control.Monad.State
import Data.Functor

import Data.IxSet hiding ( delete )
import Data.Acid
import Data.SafeCopy
import Data.Data
import Data.Lens
import Data.Lens.Template

import Server.Auth.Acid             ( UserId )
import Server.Location.Lobby        ( LobbyState(..), LobbyId )
import Server.Location.Matchmaker   ( MatchmakerState, MatchmakerId )
import Server.Location.Game         ( GameState, GameId )
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

newtype LocationAction a = LocationAction { _unLocationAction :: State LocationState a } deriving (Monad, MonadState LocationState)

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

-- internal API for manipulating individual locations

class (ChatRoom location) => Location location view where
    join    :: UserId -> location -> LocationAction Bool -- True to join, False to prevent join
    leave   :: UserId -> location -> LocationAction Bool -- not even sure what the Bool here is
    look    :: location -> view
    blank   :: location
    delete  :: location -> LocationAction Bool -- True to delete, False to prevent deletion

    join _ _ = return True
    leave _ _ = return True
    delete _ = return True

class (Location location view) => LocationHolder locationHolder location locationId view where
    lookup  :: locationId -> locationHolder -> Maybe location
    insert  :: location -> locationHolder -> locationHolder
    update  :: locationId -> location -> locationHolder -> locationHolder
    nextId  :: locationHolder -> locationId
    incId   :: locationHolder -> locationHolder

{-
deleteLocation :: LocationId -> LocationAction Bool
deleteLocation locationId@(InLobby lobbyId) = do
    locationState <- get
    shouldDelete <- delete locationId
    if shouldDelete 
        then do
            lobbyState %= \(LobbyState u lobbies) -> (LobbyState u (deleteIx lobbyId lobbies))
            LocationAction $ return shouldDelete
        else
            LocationAction $ return shouldDelete
-}

-- provided by library based on Loc definition:
-- get, put, modify, create, delete
-- modify  :: locId -> (loc -> loc) -> LocationAction loc -- this is provided by framework for any Loc
-- create  :: LocationAction locId -- calls blank, stores that
