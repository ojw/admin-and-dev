

module Framework.Location.Internal.Views.MatchmakerView where

import Data.Text                                    ( Text )
import Framework.Profile                            ( UserName )
import Framework.Location.Internal.Types.Chat       ( ChatHolder )
import Framework.Location.Internal.Types.Lobby      ( LobbyId )
import Framework.Location.Internal.Types.Matchmaker ( MatchmakerId )

data MatchmakerView = MatchmakerView
    { matchmakerId  :: MatchmakerId
    , chats         :: ChatHolder
    , capacity      :: (Int, Int) -- (min, max)
    , owner         :: UserName
    , members       :: [UserName]
    , lobbyId       :: LobbyId
    } deriving (Ord, Eq, Read, Show)

