{-# LANGUAGE RecordWildCards, MultiParamTypeClasses, OverlappingInstances #-}

module Framework.Location.Instances.View.MatchmakerView where

import Control.Monad.Error
import Data.Maybe                                   ( catMaybes, fromJust )
import Data.Functor                                 ( (<$>) )
import Data.Text                                    ( Text )
import Control.Lens

import Framework.Profile
import Framework.Location.Types
import Framework.Location.LocationAction
import Framework.Common.Classes

data MatchmakerView = MatchmakerView
    { mvMatchmakerId  :: MatchmakerId
    , mvChats         :: ChatHolder
    , mvCapacity      :: (Int, Int) -- (min, max)
    , mvOwner         :: UserName
    , mvMembers       :: [UserName]
    , mvLobbyId       :: LobbyId
    } deriving (Ord, Eq, Read, Show)

-- Probably should have used case instead of maybe... this looks a li'l silly.
instance View Matchmaker MatchmakerView LocationAction where
    view matchmaker = do
        mOwnerName <- lookupUserName $ matchmaker ^. owner
        memberIds <- getUsers $ InMatchmaker $ matchmaker ^. matchmakerId
        memberNames <- catMaybes <$> mapM lookupUserName memberIds
        maybe (throwError OtherLocationError)   (\ownerName -> return $ MatchmakerView
                { mvMatchmakerId = matchmaker ^. matchmakerId
                , mvChats = matchmaker ^. chats
                , mvCapacity = matchmaker ^. capacity
                , mvOwner = ownerName
                , mvMembers = memberNames
                , mvLobbyId = matchmaker ^. lobbyId
                }
                                                ) mOwnerName

data MatchmakerGlimpse = MatchmakerGlimpse
    { mgMatchmakerId  :: MatchmakerId
    , mgCapacity      :: (Int, Int) -- (min, max)
    , mgOwner         :: UserName
    , mgMembers       :: Int
    } deriving (Ord, Eq, Read, Show)

-- Probably should have used case instead of maybe... this looks a li'l silly.
instance View Matchmaker MatchmakerGlimpse LocationAction where
    view matchmaker = do
        mOwnerName <- lookupUserName (matchmaker ^. owner)
        members <- fmap length $ getUsers $ InMatchmaker $ matchmaker ^. matchmakerId
        maybe (throwError OtherLocationError)   (\ownerName -> return $ MatchmakerGlimpse
                { mgMatchmakerId = matchmaker ^. matchmakerId
                , mgCapacity = matchmaker ^. capacity
                , mgOwner = ownerName
                , mgMembers = members
                }
                                                ) mOwnerName
