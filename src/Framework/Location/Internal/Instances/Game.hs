{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses #-}

module Framework.Location.Internal.Instances.Game where

import Data.Functor
import Data.Lens
import Control.Monad.Reader ( asks )

import Framework.Profile
import Framework.Location.Internal.Types.Chat
import Framework.Location.Internal.Types.Game
import Framework.Location.Internal.Types.Location hiding ( userId )
import Framework.Location.Internal.Classes.Location

instance Location Game where
    canJoin _ = currentUserId >>= fmap not . inGame
    onJoin _ = return ()
    canLeave _ = currentUserId >>= fmap not . inGame
    onLeave _ = return ()
    exit = return . InLobby . _lobbyId
    chat c game = modGame (chats ^%= addChat c) (_gameId game) >> return () 
