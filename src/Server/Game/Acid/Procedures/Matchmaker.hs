{-# LANGUAGE TemplateHaskell, DeriveDataTypeable, GeneralizedNewtypeDeriving, TypeFamilies #-}

module Server.Game.Acid.Procedures.Matchmaker where

import Prelude hiding ( (.) )
import Control.Category     ( (.) )
import Data.Data
import Data.Functor         ( (<$>) )
import Data.Acid
import Data.SafeCopy
import Control.Monad.State  ( get )
import Control.Monad.Reader ( ask )
import Data.IxSet
import Data.Lens
import Data.Lens.Template

import Server.Auth.Acid   ( UserId )

import Server.Game.Acid.Types.Matchmaker
import Server.Game.Acid.GameAcid
import Server.Game.Acid.Types.Location

withMatchmaker :: (Matchmaker -> a) -> MatchmakerId -> Query (GameAcid p s o) (Maybe a)
withMatchmaker fn matchmakerId = do
    gameAcid <- ask
    case getOne $ ((gameAcid ^. matchmakerState) ^. matchmakers) @= matchmakerId of
        Nothing         -> return Nothing
        Just matchmaker -> return $ Just $ fn matchmaker 

availableCapacity' :: MatchmakerId -> MatchmakerState -> LocationState -> Maybe Int
availableCapacity' matchmakerId matchmakerState locationState =
    let mMatchmaker = getOne $ (matchmakers ^$ matchmakerState) @= matchmakerId
        mCapacity = _capacity <$> mMatchmaker
        players = length $ toList $ (locations ^$ locationState) @= (InMatchmaker matchmakerId)
    in  fmap (\x -> x - players) mCapacity

hasCapacity' :: MatchmakerId -> MatchmakerState -> LocationState -> Bool
hasCapacity' matchmakerId matchmakerState locationState =
    case availableCapacity' matchmakerId matchmakerState locationState of
        Nothing         -> False
        Just capacity   -> capacity > 0

getMatchmakerMemberIds' :: MatchmakerId -> GameAcid p s o -> Maybe [UserId]
getMatchmakerMemberIds' matchmakerId gameAcid =
    case getOne $ ((gameAcid ^. matchmakerState) ^. matchmakers) @= matchmakerId of
        Nothing         -> Nothing
        Just matchmaker -> Just $ map _userId $ toList $ ((gameAcid ^. locationState) ^. locations) @= (InMatchmaker matchmakerId)


getMatchmakerMemberIds :: MatchmakerId -> Query (GameAcid p s o) (Maybe [UserId])
getMatchmakerMemberIds matchmakerId = do
    gameAcid <- ask
    return $ getMatchmakerMemberIds' matchmakerId gameAcid

matchmakerAvailableCapacity :: MatchmakerId -> Query (GameAcid p s o) (Maybe Int)
matchmakerAvailableCapacity matchmakerId = do 
    gameAcid <- ask
    return $ availableCapacity' matchmakerId (matchmakerState ^$ gameAcid) (locationState ^$ gameAcid)

matchmakerHasCapacity :: MatchmakerId -> Query (GameAcid p s o) Bool
matchmakerHasCapacity matchmakerId = do
    gameAcid <- ask
    return $ hasCapacity' matchmakerId (matchmakerState ^$ gameAcid) (locationState ^$ gameAcid)
        
getMatchmakerOwner :: MatchmakerId -> Query (GameAcid p s o) (Maybe UserId)
getMatchmakerOwner = withMatchmaker _owner

getMatchmakerLobbyId :: MatchmakerId -> Query (GameAcid p s o) (Maybe LobbyId)
getMatchmakerLobbyId = withMatchmaker _lobbyId

-- return [] if lobby does not exist
-- returns full matchmaker, not just id
lookMatchmakers :: LobbyId -> Query (GameAcid p s o) [Matchmaker]
lookMatchmakers lobbyId = do
    gameAcid <- ask
    return $ toList $ ((gameAcid ^. matchmakerState) ^. matchmakers) @= lobbyId


--UPDATE with changed chat handling
{-
createMatchmaker :: UserId -> Int -> Int -> RoomId -> LobbyId -> Update (GameAcid p s o) MatchmakerId
createMatchmaker userId cap required roomId lobbyId = do
    gameAcid <- get
    let next = (nextMatchmaker . matchmakerState) ^$ gameAcid in
        do  matchmakerState %= (nextMatchmaker ^%= succ)
            matchmakerState %= (matchmakers ^%= updateIx next (Matchmaker next cap required userId roomId lobbyId))
            return next
-}

getMatchmaker :: MatchmakerId -> Query (GameAcid p s o) (Maybe Matchmaker)
getMatchmaker = withMatchmaker id

getMatchmakerCapacity :: MatchmakerId -> Query (GameAcid p s o) (Maybe Int)
getMatchmakerCapacity = withMatchmaker _capacity
