{-# LANGUAGE GeneralizedNewtypeDeriving, DeriveDataTypeable, TemplateHaskell, 
    MultiParamTypeClasses, FlexibleContexts, FlexibleInstances #-}

module Framework.Location.Internal.Instances.Location where

import Control.Monad hiding ( join )
import Data.Functor
import Control.Monad.State hiding ( join, modify )
import Control.Monad.Reader hiding ( join )
import Control.Monad.Error hiding ( join )
import Data.SafeCopy
import Data.Data
import Data.Acid
import Data.Lens
import Data.Lens.Template
import Data.Text                            ( Text )

import Framework.Profile ( UserId )
import Framework.Profile as Profile
import Framework.Location.Internal.Instances.Lobby as Lobby
import Framework.Location.Internal.Instances.Matchmaker as Matchmaker
import Framework.Location.Internal.Instances.Matchmaker as Matchmaker
import Framework.Location.Internal.Instances.Game as Game
import Framework.Location.Internal.Types.Chat hiding ( addChat )
import Framework.Location.Internal.Types.Location
import Framework.Location.Internal.Types.Matchmaker
import Framework.Location.Internal.Types.Game
import Framework.Location.Internal.Classes.Location
import Framework.Location.Internal.Types.Lobby
import Framework.Common.Classes ( IndexedContainer(..) )

instance Loc Location where
    canJoin (LocLobby lobby) = canJoin lobby
    canJoin (LocMatchmaker matchmaker) = canJoin matchmaker
    canJoin (LocGame game) = canJoin game

    onJoin (LocLobby lobby) = onJoin lobby
    onJoin (LocMatchmaker matchmaker) = onJoin matchmaker
    onJoin (LocGame game) = onJoin game
    
    canLeave (LocLobby lobby) = canLeave lobby
    canLeave (LocMatchmaker matchmaker) = canLeave matchmaker
    canLeave (LocGame game) = canLeave game

    onLeave (LocLobby lobby) = onLeave lobby
    onLeave (LocMatchmaker matchmaker) = onLeave matchmaker
    onLeave (LocGame game) = onLeave game

    exit (LocLobby lobby) = exit lobby
    exit (LocMatchmaker matchmaker) = exit matchmaker
    exit (LocGame game) = exit game

    chat c (LocLobby lobby) = chat c lobby
    chat c (LocMatchmaker matchmaker) = chat c matchmaker
    chat c (LocGame game) = chat c game

instance Loc LocationId where
    canJoin locationId = getLocation locationId >>= maybe (throwError LocationDoesNotExist) canJoin
    onJoin locationId = getLocation locationId >>= maybe (throwError LocationDoesNotExist) onJoin
    canLeave locationId = getLocation locationId >>= maybe (throwError LocationDoesNotExist) canLeave
    onLeave locationId = getLocation locationId >>= maybe (throwError LocationDoesNotExist) onLeave
    exit locationId = getLocation locationId >>= maybe (throwError LocationDoesNotExist) exit
    chat c locationId = getLocation locationId >>= maybe (throwError LocationDoesNotExist) (chat c)

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
