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
import Framework.Common.Classes ( IndexedContainer(..) )
import Data.IxSet ( updateIx, deleteIx, getOne, (@=) )

instance Loc Game where
    canJoin _ = currentUserId >>= fmap not . inGame
    onJoin _ = return ()
    canLeave _ = currentUserId >>= fmap not . inGame
    onLeave _ = return ()
    exit = return . InLobby . _lobbyId
    chat c game = modGame (chats ^%= addChat c) (_gameId game) >> return () 

instance IndexedContainer GameId Game GameState where
    add game (GameState games nextGameId) = (nextGameId, (GameState games' (succ nextGameId))) 
        where
            games' = updateIx nextGameId (game { _gameId = nextGameId }) games
    modify gameId f (GameState games n) = (mGame, (GameState games' n)) 
        where
            mGame = f <$> (getOne $ games @= gameId)
            games' = case mGame of
                        Just game -> updateIx gameId game games
                        Nothing -> games
    delete gameId (GameState games n) = (GameState (deleteIx gameId games) n)
