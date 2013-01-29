{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses #-}

module Framework.Location.Internal.Instances.Lobby where

import Control.Monad.State
import Data.Functor
import Data.Lens

import Framework.Location.Internal.Types.Chat
import Framework.Location.Internal.Types.Lobby
import Framework.Location.Internal.Types.Location
import Framework.Location.Internal.Classes.Location

instance (LocationAction p m) => Location Lobby p m where
    canJoin _ = return True
    onJoin _ = return ()
    canLeave _ = return True
    onLeave _ = return ()
    exit _ = InLobby <$> gets _defaultLobbyId
    chat c lobby = modLobby (chats ^%= addChat c) (_lobbyId lobby) >> return () 
