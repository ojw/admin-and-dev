{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, RecordWildCards, OverloadedStrings #-}

module Framework.Location.Internal.Instances.IndexedContainer where

import Data.Functor
import Data.Lens
import Data.Text
import Data.IxSet ( updateIx, deleteIx, getOne, (@=) )

import Framework.Profile ( UserId(..) )
import Framework.Location.Internal.Types.Chat
import Framework.Location.Internal.Types.Lobby as L
import Framework.Location.Internal.Types.Matchmaker as M
import Framework.Location.Internal.Types.Game as G
import Framework.Location.Internal.Types.Location
import Framework.Location.Internal.Classes.Location
import Framework.Common.Classes ( IndexedContainer(..), Create(..) )

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
