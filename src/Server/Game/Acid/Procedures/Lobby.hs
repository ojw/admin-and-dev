{-# LANGUAGE DeriveDataTypeable, GADTs, TemplateHaskell, GeneralizedNewtypeDeriving,
    OverloadedStrings, StandaloneDeriving, TypeFamilies, ScopedTypeVariables #-}

module Server.Game.Acid.Procedures.Lobby

where

import Prelude hiding ( (.) )
import Control.Category     ( (.) )
import Control.Monad.Reader
import Control.Monad.State      ( get )
import Data.IxSet
import Data.SafeCopy
import Data.Acid
import Data.Data
import Data.Lens
import Data.Text hiding ( map )

import Server.Auth.Acid        ( UserId )

import Server.Game.Acid.Types.Lobby
import Server.Game.Acid.Types.Location
import Server.Game.Acid.Types.Room              ( RoomId )
import Server.Game.Acid.Procedures.Room         ( createRoom )
import Server.Game.Acid.GameAcid

getLobbyRoomId' :: LobbyId -> LobbyState -> (Maybe RoomId)
getLobbyRoomId' lobbyId lobbyState = fmap _roomId $ getOne $ (lobbies ^$ lobbyState) @= lobbyId

getLobbyRoomId :: LobbyId -> Query (GameAcid p s o) (Maybe RoomId)
getLobbyRoomId lobbyId = do
    gameAcid <- ask
    return $ getLobbyRoomId' lobbyId (gameAcid ^. lobbyState)

getLobbyMemberIds :: LobbyId -> Query (GameAcid p s o) [UserId]
getLobbyMemberIds lobbyId = do
    gameAcid <- ask
    return $ map _userId $ toList $ ((gameAcid ^. locationState) ^. locations) @= (InLobby lobbyId)

lookLobbies :: Query (GameAcid p s o) [Lobby]
lookLobbies = do
    gameAcid <- ask
    return $ toList $ (lobbies . lobbyState) ^$ gameAcid

withLobby :: (Lobby -> a) -> LobbyId -> Query (GameAcid p s o) (Maybe a)
withLobby f lobbyId = do
    gameAcid <- ask
    return $ fmap f $ getOne $ (lobbies . lobbyState ^$ gameAcid) @= lobbyId

getLobbyName :: LobbyId -> Query (GameAcid p s o) (Maybe Text)
getLobbyName = withLobby _name

newLobby :: Text -> Update (GameAcid p s o) LobbyId
newLobby name = do
    gameAcid <- get
    newRoomId <- createRoom
    let ls = lobbyState ^$ gameAcid
        newLobbyId = nextLobbyId ^$ ls
    lobbyState %= (nextLobbyId ^%= succ)
    lobbyState %= (lobbies ^%= updateIx newLobbyId (Lobby newLobbyId newRoomId name))
    return newLobbyId
