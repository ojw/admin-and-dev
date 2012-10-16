{-# LANGUAGE TemplateHaskell, DeriveDataTypeable, GeneralizedNewtypeDeriving, TypeFamilies #-}

module Core.Game.Acid.Matchmaker where

import Data.Data
import Data.Functor         ( (<$>) )
import Data.Acid
import Data.SafeCopy
import Control.Monad.State  ( get )
import Control.Monad.Reader ( ask )
import Data.IxSet
import Data.Lens
import Data.Lens.Template

import Core.Auth.Acid   ( UserId )
import Core.Room.Acid   ( RoomId )

-- will need generic Options datatype for matchmaking choices
-- are all options decided before the Matchmaker is created,
-- or are some decided on in the game?
-- let's start by deciding on everything before the Matchmaker goes live

newtype MatchmakerId = MatchmakerId { _unMatchmakerId :: Int } deriving (Ord, Eq, Read, Show, Data, Typeable, Enum, SafeCopy)
$(makeLens ''MatchmakerId)

data Matchmaker = Matchmaker
    { _matchmakerId :: MatchmakerId
    , _capacity     :: Int
    , _players      :: [UserId]
    , _owner        :: UserId
    , _roomId       :: RoomId
    } deriving (Ord, Eq, Read, Show, Data, Typeable)

$(makeLens ''Matchmaker)
$(deriveSafeCopy 0 'base ''Matchmaker)

newtype Owner = Owner { _unOwner :: UserId } deriving (Ord, Eq, Read, Show, Data, Typeable)

instance Indexable Matchmaker where
    empty = ixSet [ ixFun $ \matchmaker -> [ matchmakerId ^$ matchmaker ]
                  , ixFun $ \matchmaker -> [ capacity ^$ matchmaker ]
                  , ixFun $ \matchmaker -> [ Owner $ owner ^$ matchmaker ]
                  , ixFun $ \matchmaker -> players ^$ matchmaker
                  ]

data MatchmakerState = MatchmakerState
    { _nextMatchmaker   :: MatchmakerId
    , _matchmakers      :: IxSet Matchmaker
    } deriving (Ord, Eq, Read, Show, Data, Typeable)

initialMatchmakerState :: MatchmakerState
initialMatchmakerState = MatchmakerState (MatchmakerId 1) empty

$(makeLens ''MatchmakerState)
$(deriveSafeCopy 0 'base ''MatchmakerState)

addUser :: UserId -> Matchmaker -> Matchmaker
addUser userId m = (players ^%= (userId:)) m

removeUser :: UserId -> Matchmaker -> Matchmaker
removeUser userId m = (players ^%= (filter (/= userId))) m

hasCapacity :: Matchmaker -> Bool
hasCapacity matchmaker = availableCapacity matchmaker > 0-- length (players ^$ matchmaker) < (capacity ^$ matchmaker)

availableCapacity :: Matchmaker -> Int
availableCapacity matchmaker = (capacity ^$ matchmaker) - length (players ^$ matchmaker)

createMatchmaker :: Int -> UserId -> RoomId -> Update MatchmakerState MatchmakerId
createMatchmaker cap userId roomId =
    do  matchmakerState <- get
        let next = nextMatchmaker ^$ matchmakerState in
            do  matchmakers %= updateIx next (Matchmaker next cap [userId] userId roomId)
                nextMatchmaker %= succ
                return next

-- returns list of players to relocate
deleteMatchmaker :: MatchmakerId -> Update MatchmakerState [UserId]
deleteMatchmaker matchmakerId =
    do  matchmakerState <- get
        case getOne $ (matchmakers ^$ matchmakerState) @= matchmakerId of
            Nothing         -> return []
            Just matchmaker ->
                let playersToRelocate = players ^$ matchmaker in
                    do  matchmakers %= deleteIx matchmakerId
                        return playersToRelocate

-- searches based on room *membership*, not ownership
getMatchmakerByUser :: UserId -> Query MatchmakerState (Maybe MatchmakerId)
getMatchmakerByUser userId =
    do  matchmakerState <- ask
        return $ _matchmakerId <$> (getOne $ (matchmakers ^$ matchmakerState) @= userId)

getMatchmakerByOwner :: UserId -> Query MatchmakerState (Maybe MatchmakerId)
getMatchmakerByOwner userId =
    do  matchmakerState <- ask
        return $ _matchmakerId  <$> (getOne $ (matchmakers ^$ matchmakerState) @= (Owner userId))

{-
getMatchmakerById   :: MatchmakerId -> Query MatchmakerState (Maybe Matchmaker)
getMatchmakerById matchmakerId =
    do  matchmakerState <- ask
        return $ 
-}

joinMatchmaker :: UserId -> MatchmakerId -> Update MatchmakerState (Maybe MatchmakerId)
joinMatchmaker userId matchmakerId =
    do  matchmakerState <- get
        case getOne $ (matchmakers ^$ matchmakerState) @= matchmakerId of
            Nothing         -> return Nothing
            Just matchmaker -> if hasCapacity matchmaker 
                               then do  matchmakers %= updateIx matchmakerId (addUser userId matchmaker)
                                        return $ Just matchmakerId
                               else return Nothing                                

-- returns list of players to relocate
leaveMatchmaker :: UserId -> MatchmakerId -> Update MatchmakerState [UserId]
leaveMatchmaker userId matchmakerId =
    do  matchmakerState <- get
        case getOne $ (matchmakers ^$ matchmakerState) @= matchmakerId of
            Nothing         -> return []
            Just matchmaker -> if userId == (owner ^$ matchmaker)
                               then do  deleteMatchmaker matchmakerId
                               else do  matchmakers %= updateIx matchmakerId (removeUser userId matchmaker)
                                        return [userId]

lookMatchmakers :: Query MatchmakerState [Matchmaker]
lookMatchmakers = 
    do  matchmakerState <- ask
        return $ toList $ matchmakers ^$ matchmakerState        

getOwner :: MatchmakerId -> Query MatchmakerState (Maybe UserId)
getOwner matchmakerId =
    do  matchmakerState <- ask
        return $ _owner <$> (getOne $ (matchmakers ^$ matchmakerState) @= matchmakerId)

$(makeAcidic ''MatchmakerState ['createMatchmaker, 'deleteMatchmaker, 'getMatchmakerByUser, 'joinMatchmaker, 'leaveMatchmaker, 'lookMatchmakers, 'getOwner, 'getMatchmakerByOwner])
