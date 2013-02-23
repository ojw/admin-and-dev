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

data LobbyOptions = LobbyOptions
    { lobbyOptionsName          :: Maybe Text
    , lobbyOptionsDescription   :: Maybe Text
    }

instance Create LobbyOptions Lobby where
    blank = Lobby "" "" (LobbyId 0) []
    update options lobby = lobby 
        { _name = maybe (_name lobby) id (lobbyOptionsName options)
        , _description = maybe (_description lobby) id (lobbyOptionsDescription options)
        }
