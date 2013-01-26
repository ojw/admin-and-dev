{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses #-}

module Framework.Game.Location.Internal.Instances.Game where

import Data.Functor
import Data.Lens
import Control.Monad.Reader ( asks )

import Framework.Profile.Profile
import Framework.Game.Location.Internal.Types.Chat
import Framework.Game.Location.Internal.Types.Game
import Framework.Game.Location.Internal.Types.Location hiding ( userId )
import Framework.Game.Location.Internal.Classes.Location

instance (LocationAction p m) => Location Game p m where
    canJoin _ = asks userId >>= fmap not . inGame
    onJoin _ = return ()
    canLeave _ = asks userId >>= fmap not . inGame
    onLeave _ = return ()
    exit = return . InLobby . _lobbyId
    chat c game = modGame (chats ^%= addChat c) (_gameId game) >> return () 
