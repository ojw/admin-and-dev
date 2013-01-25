{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses #-}

module Framework.Game.Location.Internal.Instances.Game where

import Data.Functor
import Data.Lens

import Framework.Game.Location.Internal.Types.Chat
import Framework.Game.Location.Internal.Types.Game
import Framework.Game.Location.Internal.Types.Location

instance (LocationAction p m) => Location Game p m where
    canJoin _ = return True
    onJoin _ = return ()
    canLeave _ = return True
    onLeave _ = return ()
    exit = return . InLobby . _lobbyId
    chat c game = modGame (chats ^%= (addChat c)) (_gameId game) >> return () 
