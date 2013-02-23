{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses #-}

module Framework.Location.Internal.Instances.Lobby where

import Control.Monad.State
import Data.Functor
import Data.Lens

import Framework.Location.Internal.Types.Chat
import Framework.Location.Internal.Types.Lobby
import Framework.Location.Internal.Types.Location
import Framework.Location.Internal.Classes.Location
import Framework.Common.Classes ( IndexedContainer(..) )
import Data.IxSet ( updateIx, deleteIx, getOne, (@=) )

instance Loc Lobby where
    canJoin _ = return True
    onJoin _ = return ()
    canLeave _ = return True
    onLeave _ = return ()
    exit _ = InLobby <$> gets _defaultLobbyId
    chat c lobby = modLobby (chats ^%= addChat c) (_lobbyId lobby) >> return () 

instance IndexedContainer LobbyId Lobby LobbyState where
    add lobby (LobbyState lobbies nextLobbyId) = (nextLobbyId, (LobbyState lobbies' (succ nextLobbyId))) 
        where
            lobbies' = updateIx nextLobbyId (lobby { _lobbyId = nextLobbyId }) lobbies
    modify lobbyId f (LobbyState lobbies n) = (mLobby, (LobbyState lobbies' n)) 
        where
            mLobby = f <$> (getOne $ lobbies @= lobbyId)
            lobbies' = case mLobby of
                        Just lobby -> updateIx lobbyId lobby lobbies
                        Nothing -> lobbies
    delete lobbyId (LobbyState lobbies n) = (LobbyState (deleteIx lobbyId lobbies) n)
