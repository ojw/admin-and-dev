{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, RecordWildCards, OverloadedStrings #-}

module Framework.Location.Internal.Instances.Lobby where

import Control.Monad.State
import Data.Functor
import Data.Lens
import Data.Text
import Data.IxSet ( updateIx, deleteIx, getOne, (@=) )

import Framework.Location.Internal.Types.Chat
import Framework.Location.Internal.Types.Lobby
import Framework.Location.Internal.Types.Location
import Framework.Location.Internal.Classes.Location
import Framework.Common.Classes ( IndexedContainer(..), Create(..) )

instance Loc Lobby where
    canJoin _ = return True
    onJoin _ = return ()
    canLeave _ = return True
    onLeave _ = return ()
    exit _ = InLobby <$> getDefaultLobbyId
    chat c lobby = updateLobby (_lobbyId lobby) ((chats ^%= addChat c) lobby) >> return ()
