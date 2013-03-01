{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, RecordWildCards, OverloadedStrings #-}

module Framework.Location.Instances.IndexedContainer where

import Data.Functor
import Control.Lens
import Data.IxSet ( updateIx, deleteIx, getOne, (@=) )

import Framework.Location.Types
import Framework.Profile ( UserId(..) )
import Framework.Common.Classes ( IndexedContainer(..), Create(..) )

instance IndexedContainer LobbyId Lobby LobbyState where
    add lobby (LobbyState lobbies nextLobbyId) = (nextLobbyId, (LobbyState lobbies' (succ nextLobbyId))) 
        where
            lobbies' = updateIx nextLobbyId (lobby & lobbyId .~ nextLobbyId) lobbies
    modify lobbyId f (LobbyState lobbies n) = (mLobby, (LobbyState lobbies' n)) 
        where
            mLobby = f <$> (getOne $ lobbies @= lobbyId)
            lobbies' = case mLobby of
                        Just lobby -> updateIx lobbyId lobby lobbies
                        Nothing -> lobbies
    delete lobbyId (LobbyState lobbies n) = (LobbyState (deleteIx lobbyId lobbies) n)

instance IndexedContainer MatchmakerId Matchmaker MatchmakerState where
    add matchmaker (MatchmakerState matchmakers nextMatchmakerId) = (nextMatchmakerId, (MatchmakerState matchmakers' (succ nextMatchmakerId))) 
        where
            matchmakers' = updateIx nextMatchmakerId (matchmaker & matchmakerId .~ nextMatchmakerId) matchmakers
    modify matchmakerId f (MatchmakerState matchmakers n) = (mMatchmaker, (MatchmakerState matchmakers' n)) 
        where
            mMatchmaker = f <$> (getOne $ matchmakers @= matchmakerId)
            matchmakers' = case mMatchmaker of
                        Just matchmaker -> updateIx matchmakerId matchmaker matchmakers
                        Nothing -> matchmakers
    delete matchmakerId (MatchmakerState matchmakers n) = (MatchmakerState (deleteIx matchmakerId matchmakers) n)

instance IndexedContainer GameId Game GameState where
    add game (GameState games nextGameId) = (nextGameId, (GameState games' (succ nextGameId))) 
        where
            games' = updateIx nextGameId (game & gameId .~ nextGameId) games
    modify gameId f (GameState games n) = (mGame, (GameState games' n)) 
        where
            mGame = f <$> (getOne $ games @= gameId)
            games' = case mGame of
                        Just game -> updateIx gameId game games
                        Nothing -> games
    delete gameId (GameState games n) = (GameState (deleteIx gameId games) n)

instance IndexedContainer LocationId Location LocationState where
    add (LocLobby lobby) locationState = (InLobby lobbyId, locationState')
        where
            (lobbyId, lobbyState') = add lobby (view lobbyState locationState)
            locationState' = locationState & lobbyState .~ lobbyState'
    add (LocMatchmaker matchmaker) locationState = (InMatchmaker matchmakerId, locationState')
        where
            (matchmakerId, matchmakerState') = add matchmaker (view matchmakerState locationState)
            locationState' = locationState & matchmakerState .~ matchmakerState'
    add (LocGame game) locationState = (InGame gameId, locationState')
        where
            (gameId, gameState') = add game (view gameState locationState)
            locationState' = set gameState gameState' locationState
    modify (InLobby lobbyId) f locationState = (LocLobby <$> mLobby', locationState')
        where
            f' = \lobby -> unLocLobby (f (LocLobby lobby))
            (mLobby', lobbyState') = modify lobbyId f' (locationState ^. lobbyState) :: (Maybe Lobby, LobbyState)
            locationState' = locationState & lobbyState .~ lobbyState'
    modify (InMatchmaker matchmakerId) f locationState = (LocMatchmaker <$> mMatchmaker', locationState')
        where
            f' = \matchmaker -> unLocMatchmaker (f (LocMatchmaker matchmaker))
            (mMatchmaker', matchmakerState') = modify matchmakerId f' (locationState ^. matchmakerState)
            locationState' = locationState & matchmakerState .~ matchmakerState'
    modify (InGame gameId) f locationState = (LocGame <$> mGame', locationState')
        where
            f' = \game -> unLocGame (f (LocGame game))
            (mGame', gameState') = modify gameId f' (locationState ^. gameState) :: (Maybe Game, GameState)
            locationState' = locationState & gameState .~ gameState'
    delete (InLobby lobbyId) locationState = locationState'
        where
            lobbyState' = delete lobbyId (locationState ^. lobbyState)
            locationState' = locationState & lobbyState .~ lobbyState'
    delete (InMatchmaker matchmakerId) locationState = locationState'
        where
            matchmakerState' = delete matchmakerId (locationState ^. matchmakerState)
            locationState' = locationState & matchmakerState .~ matchmakerState'
    delete (InGame gameId) locationState = locationState'
        where
            gameState' = delete gameId (locationState ^. gameState)
            locationState' = locationState & gameState .~ gameState'
