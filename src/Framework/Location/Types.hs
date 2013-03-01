{-# LANGUAGE GeneralizedNewtypeDeriving, DeriveDataTypeable, TemplateHaskell, 
    MultiParamTypeClasses, FlexibleContexts, FlexibleInstances, TypeFamilies,
    OverloadedStrings, FunctionalDependencies #-}

module Framework.Location.Types where

import Data.SafeCopy
import Data.Data
import Control.Lens
import Data.IxSet
import Data.Text

import Framework.Profile ( UserId, UserName )

type Chat = (UserName, Text)
type ChatHolder = [Chat]

newtype LobbyId = LobbyId Int deriving (Ord, Eq, Read, Show, Data, Typeable, SafeCopy, Enum)
newtype MatchmakerId = MatchmakerId Int deriving (Ord, Eq, Read, Show, Data, Typeable, SafeCopy, Enum)
newtype GameId = GameId Int deriving (Ord, Eq, Read, Show, Data, Typeable, SafeCopy, Enum)

data Lobby = Lobby
    { _lobbyName         :: Text
    , _lobbyDescription  :: Text
    , _lobbyLobbyId      :: LobbyId
    , _lobbyChats        :: ChatHolder
    } deriving (Ord, Eq, Read, Show, Data, Typeable)

data Matchmaker = Matchmaker
    { _matchmakerMatchmakerId  :: MatchmakerId
    , _matchmakerChats         :: ChatHolder
    , _matchmakerCapacity      :: (Int, Int) -- (min, max)
    , _matchmakerOwner         :: UserId
    , _matchmakerLobbyId       :: LobbyId
    } deriving (Ord, Eq, Read, Show, Data, Typeable)

data Game = Game
    { _gameGameId        :: GameId
    , _gameMatchmakerId  :: MatchmakerId
    , _gameLobbyId       :: LobbyId
    , _gameChats         :: ChatHolder
    } deriving (Ord, Eq, Read, Show, Data, Typeable)

inferIxSet "Lobbies" ''Lobby 'noCalcs [''LobbyId]
inferIxSet "Matchmakers" ''Matchmaker 'noCalcs [''MatchmakerId, ''UserId, ''LobbyId]
inferIxSet "Games" ''Game 'noCalcs [''GameId, ''MatchmakerId, ''LobbyId]

data LobbyState = LobbyState
    { _lsLobbies       :: Lobbies
    , _lsStateNextLobbyId   :: LobbyId
    } deriving (Ord, Eq, Read, Show, Data, Typeable)

data MatchmakerState = MatchmakerState
    { _msMatchmakers      :: Matchmakers
    , _msStateNextMatchmakerId :: MatchmakerId
    } deriving (Ord, Eq, Read, Show, Data, Typeable)

data GameState = GameState
    { _gsGames        :: Games
    , _gsNextGameId   :: GameId
    } deriving (Ord, Eq, Read, Show, Data, Typeable)

data LocationId = InLobby LobbyId | InMatchmaker MatchmakerId | WatchingGame GameId | InGame GameId
    deriving (Ord, Eq, Read, Show, Data, Typeable)

data UserLocation = UserLocation
    { _ulUserId        :: UserId
    , _ulLocationId    :: LocationId
    } deriving (Ord, Eq, Read, Show, Data, Typeable)

inferIxSet "UserLocations" ''UserLocation 'noCalcs [''UserId, ''LocationId]

data Location
    = LocLobby { unLocLobby :: Lobby }
    | LocMatchmaker { unLocMatchmaker :: Matchmaker }
    | LocGame { unLocGame :: Game }
    deriving (Ord, Eq, Read, Show, Data, Typeable)

data LocationState = LocationState
    { _locsUserLocations    :: UserLocations
    , _locsDefaultLobbyId   :: LobbyId
    , _locsLobbyState       :: LobbyState
    , _locsMatchmakerState  :: MatchmakerState
    , _locsGameState        :: GameState
    } deriving (Ord, Eq, Read, Show, Data, Typeable)

makeFields ''Lobby
makeFields ''Matchmaker
makeFields ''Game
makeFields ''LobbyState
makeFields ''MatchmakerState
makeFields ''GameState
makeFields ''UserLocation
makeFields ''LocationState

deriveSafeCopy 0 'base ''Lobby
deriveSafeCopy 0 'base ''LobbyState
deriveSafeCopy 0 'base ''Matchmaker
deriveSafeCopy 0 'base ''MatchmakerState
deriveSafeCopy 0 'base ''Game
deriveSafeCopy 0 'base ''GameState
deriveSafeCopy 0 'base ''LocationId
deriveSafeCopy 0 'base ''UserLocation
deriveSafeCopy 0 'base ''Location
deriveSafeCopy 0 'base ''LocationState

addChat :: Chat -> ChatHolder -> ChatHolder
addChat = (:) 
