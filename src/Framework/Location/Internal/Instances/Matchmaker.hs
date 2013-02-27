{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, RecordWildCards #-}

module Framework.Location.Internal.Instances.Matchmaker where

import Control.Monad.Reader
import Data.Functor
import Data.Lens

import Framework.Profile as Profile
import Framework.Location.Internal.Types.Chat
import Framework.Location.Internal.Types.Lobby ( LobbyId(..) )
import Framework.Location.Internal.Types.Matchmaker
import Framework.Location.Internal.Types.Location
import Framework.Location.Internal.Classes.Location
import Framework.Common.Classes ( IndexedContainer(..), Create(..) )
import Data.IxSet ( updateIx, deleteIx, getOne, (@=) )

instance Loc Matchmaker where
    canJoin matchmaker = do
        users <- getUsers $ InMatchmaker $ _matchmakerId matchmaker
        return $ length users < snd (_capacity matchmaker)
    onJoin _ = return ()
    canLeave _ = return True
    onLeave matchmaker = do
        userId <- currentUserId
        if userId == _owner matchmaker
        then do
            users <- getUsers $ InMatchmaker $ _matchmakerId matchmaker
            exit <- exit matchmaker            
            mapM_ (setLocation exit) users
        else return ()
    exit = return . InLobby . _lobbyId
    chat c matchmaker = updateMatchmaker (_matchmakerId matchmaker) ((chats ^%= addChat c) matchmaker) >> return ()

instance IndexedContainer MatchmakerId Matchmaker MatchmakerState where
    add matchmaker (MatchmakerState matchmakers nextMatchmakerId) = (nextMatchmakerId, (MatchmakerState matchmakers' (succ nextMatchmakerId))) 
        where
            matchmakers' = updateIx nextMatchmakerId (matchmaker { _matchmakerId = nextMatchmakerId }) matchmakers
    modify matchmakerId f (MatchmakerState matchmakers n) = (mMatchmaker, (MatchmakerState matchmakers' n)) 
        where
            mMatchmaker = f <$> (getOne $ matchmakers @= matchmakerId)
            matchmakers' = case mMatchmaker of
                        Just matchmaker -> updateIx matchmakerId matchmaker matchmakers
                        Nothing -> matchmakers
    delete matchmakerId (MatchmakerState matchmakers n) = (MatchmakerState (deleteIx matchmakerId matchmakers) n)

data MatchmakerOptions = MatchmakerOptions
    { matchmakerOptionsCapacity :: Maybe (Int, Int)
    }

instance Create MatchmakerOptions Matchmaker where
    blank = Matchmaker (MatchmakerId 0) [] (2,2) (UserId 0) (LobbyId 0)
    update options matchmaker = matchmaker { _capacity = maybe (_capacity matchmaker) id (matchmakerOptionsCapacity options) }
