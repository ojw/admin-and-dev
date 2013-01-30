{-# LANGUAGE GeneralizedNewtypeDeriving, DeriveDataTypeable, TemplateHaskell, 
    MultiParamTypeClasses, FlexibleContexts, FlexibleInstances #-}

module Framework.Location.Internal.Instances.Location where

import Control.Monad hiding ( join )
import Data.Functor
import Control.Monad.State hiding ( join )
import Control.Monad.Reader hiding ( join )
import Control.Monad.Error hiding ( join )
import Data.SafeCopy
import Data.Data
import Data.Acid
import Data.Lens
import Data.Lens.Template
import Data.IxSet
import Data.Text                            ( Text )

import Framework.Auth                  ( UserId )
import Framework.Profile as Profile
import Framework.Location.Internal.Instances.Lobby as Lobby
import Framework.Location.Internal.Instances.Matchmaker as Matchmaker
import Framework.Location.Internal.Instances.Matchmaker as Matchmaker
import Framework.Location.Internal.Instances.Game as Game
import Framework.Location.Internal.Types.Chat hiding ( addChat )
import Framework.Location.Internal.Types.Location
import Framework.Location.Internal.Classes.Location

instance Location LocationId where
    canJoin (InLobby lobbyId) = getLobby lobbyId >>= maybe (throwError LocationDoesNotExist) canJoin
    canJoin (InMatchmaker matchmakerId) = getMatchmaker matchmakerId >>= maybe (throwError LocationDoesNotExist) canJoin
    canJoin (InGame gameId) = getGame gameId >>= maybe (throwError LocationDoesNotExist) canJoin
    canJoin (WatchingGame gameId) = getGame gameId >>= maybe (throwError LocationDoesNotExist) canJoin
    onJoin (InLobby lobbyId) = getLobby lobbyId >>= maybe (throwError LocationDoesNotExist) onJoin
    onJoin (InMatchmaker matchmakerId) = getMatchmaker matchmakerId >>= maybe (throwError LocationDoesNotExist) onJoin
    onJoin (InGame gameId) = getGame gameId >>= maybe (throwError LocationDoesNotExist) onJoin
    onJoin (WatchingGame gameId) = getGame gameId >>= maybe (throwError LocationDoesNotExist) onJoin
    canLeave (InLobby lobbyId) = getLobby lobbyId >>= maybe (throwError LocationDoesNotExist) canLeave
    canLeave (InMatchmaker matchmakerId) = getMatchmaker matchmakerId >>= maybe (throwError LocationDoesNotExist) canLeave
    canLeave (InGame gameId) = getGame gameId >>= maybe (throwError LocationDoesNotExist) canLeave
    canLeave (WatchingGame gameId) = getGame gameId >>= maybe (throwError LocationDoesNotExist) canLeave
    onLeave (InLobby lobbyId) = getLobby lobbyId >>= maybe (throwError LocationDoesNotExist) onLeave
    onLeave (InMatchmaker matchmakerId) = getMatchmaker matchmakerId >>= maybe (throwError LocationDoesNotExist) onLeave
    onLeave (InGame gameId) = getGame gameId >>= maybe (throwError LocationDoesNotExist) onLeave
    onLeave (WatchingGame gameId) = getGame gameId >>= maybe (throwError LocationDoesNotExist) onLeave
    exit (InLobby lobbyId) = getLobby lobbyId >>= maybe (throwError LocationDoesNotExist) exit
    exit (InMatchmaker matchmakerId) = getMatchmaker matchmakerId >>= maybe (throwError LocationDoesNotExist) exit
    exit (InGame gameId) = getGame gameId >>= maybe (throwError LocationDoesNotExist) exit
    exit (WatchingGame gameId) = getGame gameId >>= maybe (throwError LocationDoesNotExist) exit

    chat c (InLobby lobbyId) = getLobby lobbyId >>= maybe (throwError LocationDoesNotExist) (chat c)
    chat c (InMatchmaker matchmakerId) = getMatchmaker matchmakerId >>= maybe (throwError LocationDoesNotExist) (chat c)
    chat c (InGame gameId) = getGame gameId >>= maybe (throwError LocationDoesNotExist) (chat c)
    chat c (WatchingGame gameId) = getGame gameId >>= maybe (throwError LocationDoesNotExist) (chat c)
