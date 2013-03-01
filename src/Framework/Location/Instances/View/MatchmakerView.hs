{-# LANGUAGE RecordWildCards, MultiParamTypeClasses #-}

module Framework.Location.Instances.View.MatchmakerView where

import Control.Monad.Error
import Data.Maybe                                   ( catMaybes, fromJust )
import Data.Functor                                 ( (<$>) )
import Data.Text                                    ( Text )
import Control.Lens

import Framework.Profile                            ( UserName, lookupUserName )
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
