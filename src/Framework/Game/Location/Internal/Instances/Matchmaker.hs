{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses #-}

module Framework.Game.Location.Internal.Instances.Matchmaker where

import Control.Monad.Reader
import Data.Functor
import Data.Lens

import Framework.Profile.Profile as Profile
import Framework.Game.Location.Internal.Types.Chat
import Framework.Game.Location.Internal.Types.Matchmaker
import Framework.Game.Location.Internal.Types.Location

instance (LocationAction p m) => Location Matchmaker p m where
    canJoin matchmaker = do
        users <- getUsers $ InMatchmaker $ _matchmakerId matchmaker
        return $ length users < snd (_capacity matchmaker)
    onJoin _ = return ()
    canLeave _ = return True
    onLeave matchmaker = do
        userId <- asks Profile.userId
        if userId == _owner matchmaker
        then do
            users <- getUsers $ InMatchmaker $ _matchmakerId matchmaker
            exit <- exit matchmaker            
            mapM_ (setLocation exit) users
        else return ()
    exit = return . InLobby . _lobbyId
    chat c matchmaker = modMatchmaker (chats ^%= (addChat c)) (_matchmakerId matchmaker) >> return ()

