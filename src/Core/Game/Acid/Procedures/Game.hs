{-# LANGUAGE MultiParamTypeClasses, DeriveDataTypeable, GeneralizedNewtypeDeriving, Rank2Types, StandaloneDeriving, FlexibleContexts #-}

module Core.Game.Acid.Procedures.Game where

import Control.Monad.Reader ( ask )
import Control.Monad.State  ( get )
import Data.Acid
import Data.Data
import Data.Lens
import Data.IxSet
import Data.SafeCopy    ( SafeCopy )

import Core.Auth.Acid               ( UserId )
import Core.Game.Acid.Types.Room    ( RoomId )
import Core.Game.Acid.Types.Lobby   ( LobbyId)
import Core.Game.Acid.Types.Game
import Core.Game.Acid.GameAcid

dummy :: (SafeCopy (GameState p s), SafeCopy o) => Update (GameAcid p s o) ()
dummy = return ()

getGameRoomId 
    ::  ( Ord p, Ord s, Typeable p, Typeable s)
    =>  GameId -> Query (GameAcid p s o) (Maybe RoomId)
getGameRoomId gameId = do
    gameAcid <- ask
    case getOne $ (_games (gameState ^$ gameAcid)) @= gameId of
        Nothing -> return Nothing
        Just g  -> return $ Just $ _roomId g
