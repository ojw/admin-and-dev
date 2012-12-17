

module Server.Location.LocationInstances where

import Server.Location.Lobby
import Server.Location.Matchmaker
import Server.Location.Game

import Server.Location.Location

instance Location Lobby LobbyState LobbyView where
    blank = blankLobby
    look lobby = lobby
