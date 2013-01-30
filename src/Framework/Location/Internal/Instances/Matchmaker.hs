{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses #-}

module Framework.Location.Internal.Instances.Matchmaker where

import Control.Monad.Reader
import Data.Functor
import Data.Lens

import Framework.Profile as Profile
import Framework.Location.Internal.Types.Chat
import Framework.Location.Internal.Types.Matchmaker
import Framework.Location.Internal.Types.Location
import Framework.Location.Internal.Classes.Location

instance Location Matchmaker where
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
    chat c matchmaker = modMatchmaker (chats ^%= addChat c) (_matchmakerId matchmaker) >> return ()
