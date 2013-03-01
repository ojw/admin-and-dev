{-# LANGUAGE GeneralizedNewtypeDeriving, DeriveDataTypeable, TemplateHaskell, 
    MultiParamTypeClasses, FlexibleContexts, FlexibleInstances #-}

module Framework.Location.Instances.Location where

import Control.Monad hiding ( join )
import Data.Functor
import Control.Monad.State hiding ( join, modify )
import Control.Monad.Reader hiding ( join )
import Control.Monad.Error hiding ( join )
import Data.SafeCopy
import Data.Data
import Control.Lens

import Framework.Profile ( UserId )
import Framework.Profile as Profile
import Framework.Location.Classes.Location
import Framework.Location.LocationAction
import Framework.Location.Types
import Framework.Common.Classes ( IndexedContainer(..), Create(..) )

instance Loc Lobby where
    canJoin _ = return True
    onJoin _ = return ()
    canLeave _ = return True
    onLeave _ = return ()
    exit _ = InLobby <$> getDefaultLobbyId
    chat c lobby = updateLobby (lobby ^. lobbyId) ((chats %~ addChat c) lobby) >> return ()

instance Loc Matchmaker where
    canJoin matchmaker = do
        users <- getUsers $ InMatchmaker $ matchmaker ^. matchmakerId
        return $ length users < snd (matchmaker ^. capacity)
    onJoin _ = return ()
    canLeave _ = return True
    onLeave matchmaker = do
        userId <- currentUserId
        if userId == matchmaker ^. owner
        then do
            users <- getUsers $ InMatchmaker $ matchmaker ^. matchmakerId
            exit <- exit matchmaker            
            mapM_ (setLocation exit) users
        else return ()
    exit = return . InLobby . (view lobbyId)
    chat c matchmaker = updateMatchmaker (matchmaker ^. matchmakerId) ((chats %~ addChat c) matchmaker) >> return () 

instance Loc Game where
    canJoin _ = currentUserId >>= fmap not . inGame
    onJoin _ = return ()
    canLeave _ = currentUserId >>= fmap not . inGame
    onLeave _ = return ()
    exit = return . InLobby . (view lobbyId)
    chat c game = updateGame (game ^. gameId) ((chats %~ addChat c) game) >> return ()

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
