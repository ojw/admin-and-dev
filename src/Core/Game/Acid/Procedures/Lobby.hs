{-# LANGUAGE DeriveDataTypeable, GADTs, TemplateHaskell, GeneralizedNewtypeDeriving,
    OverloadedStrings, StandaloneDeriving, TypeFamilies, ScopedTypeVariables #-}

module Core.Game.Acid.Procedures.Lobby

where

import Control.Monad.Reader
import Data.IxSet
import Data.SafeCopy
import Data.Acid
import Data.Data
import Data.Lens
import Data.Lens.Template

import Core.Auth.Acid        ( UserId )

import Core.Game.Acid.Types.Lobby
import Core.Game.Acid.Types.Location
import Core.Game.Acid.Types.Room              ( RoomId )
import Core.Game.Acid.GameAcid

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
