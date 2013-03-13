{-# LANGUAGE MultiParamTypeClasses, OverloadedStrings, Rank2Types #-}

module Common.Location.Instances.Create where

import Data.Text hiding ( empty )
import Control.Lens
import Data.Maybe ( fromMaybe )
import Data.IxSet

import Framework.Profile
import Common.Classes
import Common.Location.Types

data LobbyOptions = LobbyOptions
    { lobbyOptionsName          :: Maybe Text
    , lobbyOptionsDescription   :: Maybe Text
    }

(??~) :: Lens s s a a -> (Maybe a) -> s -> s
(??~) lens maybeA s = s & lens .~ fromMaybe (Control.Lens.view lens s) maybeA

instance Create LobbyOptions Lobby where
    blank = Lobby "" "" (LobbyId 0) []
    update options lobby = lobby & name ??~ lobbyOptionsName options
                                 & description ??~ lobbyOptionsDescription options

data MatchmakerOptions = MatchmakerOptions
    { matchmakerOptionsCapacity :: Maybe (Int, Int)
    }

instance Create MatchmakerOptions Matchmaker where
    blank = Matchmaker (MatchmakerId 0) [] (2,2) (UserId 0) (LobbyId 0)
    update options matchmaker = matchmaker & capacity ??~ matchmakerOptionsCapacity options

data GameOptions = GameOptions

instance Create GameOptions Game where  
    blank = Game (GameId 0) (MatchmakerId 0) (LobbyId 0) []
    update options = id

data LocationOptions = LOLobby LobbyOptions | LOMatchmaker MatchmakerOptions | LOGame GameOptions

-- Create class doesn't work as well for Locations in general.
-- Blank has to chose which type to be, so I defaulted to Lobby.
-- Update only makes sense if the options and object types match up.
-- I think this is guaranteed by functional dependencies, but I added a pattern match that does nothing in case a mismatched pair is passed to update.
instance Create LocationOptions Location where
    blank = LocLobby (blank :: Lobby)
    update (LOLobby lobbyOptions) (LocLobby lobby) = LocLobby $ update lobbyOptions lobby
    update (LOMatchmaker matchmakerOptions) (LocMatchmaker matchmaker) = LocMatchmaker $ update matchmakerOptions matchmaker
    update (LOGame gameOptions) (LocGame game) = LocGame $ update gameOptions game
    update _ loc = loc


initialMatchmakerState :: MatchmakerState
initialMatchmakerState = MatchmakerState empty (MatchmakerId 1)

initialLobbyState :: LobbyState
initialLobbyState = LobbyState (updateIx (LobbyId 0) (update (LobbyOptions (Just "Default Lobby") (Just "The default lobby.")) blank) empty) (LobbyId 1)

initialGameState :: GameState
initialGameState = GameState empty (GameId 1)

initialLocationState :: LocationState
initialLocationState = LocationState empty (LobbyId 0) initialLobbyState initialMatchmakerState initialGameState
