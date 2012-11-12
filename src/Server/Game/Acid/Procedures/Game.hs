{-# LANGUAGE MultiParamTypeClasses, DeriveDataTypeable, GeneralizedNewtypeDeriving, Rank2Types, StandaloneDeriving, FlexibleContexts #-}

module Server.Game.Acid.Procedures.Game where

import Control.Monad.Reader ( ask )
import Control.Monad.State  ( get )
import Data.Acid
import Data.Data
import Data.Lens
import Data.IxSet
import Data.SafeCopy    ( SafeCopy )

import Server.Auth.Acid               ( UserId )
import Server.Game.Acid.Types.Room    ( RoomId )
import Server.Game.Acid.Types.Lobby   ( LobbyId)
import Server.Game.Acid.Types.Game
import Server.Game.Acid.GameAcid

dummy :: (SafeCopy (GameState p s o)) => Update (GameAcid p s o) ()
dummy = return ()

withGame 
    ::  (Ord p, Ord s, Ord o, Typeable p, Typeable s, Typeable o)
    =>  (Game p s o -> a) -> GameId -> Query (GameAcid p s o) (Maybe a)
withGame f gameId = do
    gameAcid <- ask
    case getOne $ (_games (gameState ^$ gameAcid)) @= gameId of
        Nothing -> return Nothing
        Just g  -> return $ Just $ f g

getGameRoomId 
    ::  (Ord p, Ord s, Ord o, Typeable p, Typeable s, Typeable o)
    =>  GameId -> Query (GameAcid p s o) (Maybe RoomId)
getGameRoomId gameId = withGame _roomId gameId

getGameLobbyId
    ::  (Ord p, Ord s, Ord o, Typeable p, Typeable s, Typeable o)
    =>  GameId -> Query (GameAcid p s o) (Maybe LobbyId)
getGameLobbyId gameId = withGame _lobbyId gameId
