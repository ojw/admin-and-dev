{-# LANGUAGE RecordWildCards, MultiParamTypeClasses #-}

module Framework.Location.Internal.Views.MatchmakerView where

import Data.Maybe                                   ( catMaybes, fromJust )
import Data.Functor                                 ( (<$>) )
import Data.Text                                    ( Text )
import Framework.Profile                            ( UserName, lookupUserName )
import Framework.Location.Internal.Types.Chat       ( ChatHolder )
import Framework.Location.Internal.Types.Lobby      ( LobbyId )
import Framework.Location.Internal.Types.Matchmaker ( MatchmakerId, Matchmaker(..) )
import Framework.Location.Internal.Types.Location
import Framework.Location.Internal.Classes.View ( View(..) )

data MatchmakerView = MatchmakerView
    { matchmakerId  :: MatchmakerId
    , chats         :: ChatHolder
    , capacity      :: (Int, Int) -- (min, max)
    , owner         :: UserName
    , members       :: [UserName]
    , lobbyId       :: LobbyId
    } deriving (Ord, Eq, Read, Show)

-- fromJust is bad... should notice missing username and throw an error probably
instance View Matchmaker MatchmakerView where
    view matchmaker@Matchmaker{..} = do
        ownerName <- lookupUserName _owner
        memberIds <- getUsers $ InMatchmaker _matchmakerId
        memberNames <- catMaybes <$> mapM lookupUserName memberIds
        return $ MatchmakerView
            { matchmakerId = _matchmakerId
            , chats = _chats
            , capacity = _capacity
            , owner = fromJust ownerName
            , members = memberNames
            , lobbyId = _lobbyId
            }
