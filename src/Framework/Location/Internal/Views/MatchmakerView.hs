{-# LANGUAGE RecordWildCards, MultiParamTypeClasses #-}

module Framework.Location.Internal.Views.MatchmakerView where

import Control.Monad.Error
import Data.Maybe                                   ( catMaybes, fromJust )
import Data.Functor                                 ( (<$>) )
import Data.Text                                    ( Text )
import Framework.Profile                            ( UserName, lookupUserName )
import Framework.Location.Internal.Types.Chat       ( ChatHolder )
import Framework.Location.Internal.Types.Lobby      ( LobbyId )
import Framework.Location.Internal.Types.Matchmaker ( MatchmakerId, Matchmaker(..) )
import Framework.Location.Internal.Types.Location
import Framework.Common.Classes

data MatchmakerView = MatchmakerView
    { matchmakerId  :: MatchmakerId
    , chats         :: ChatHolder
    , capacity      :: (Int, Int) -- (min, max)
    , owner         :: UserName
    , members       :: [UserName]
    , lobbyId       :: LobbyId
    } deriving (Ord, Eq, Read, Show)

-- Probably should have used case instead of maybe... this looks a li'l silly.
instance View Matchmaker MatchmakerView LocationAction where
    view matchmaker@Matchmaker{..} = do
        mOwnerName <- lookupUserName _owner
        memberIds <- getUsers $ InMatchmaker _matchmakerId
        memberNames <- catMaybes <$> mapM lookupUserName memberIds
        maybe (throwError OtherLocationError)   (\ownerName -> return $ MatchmakerView
                { matchmakerId = _matchmakerId
                , chats = _chats
                , capacity = _capacity
                , owner = ownerName
                , members = memberNames
                , lobbyId = _lobbyId
                }
                                                ) mOwnerName
