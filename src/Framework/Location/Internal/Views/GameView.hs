

module Framework.Location.Internal.Views.GameView where

import Data.Text                                    ( Text )
import Framework.Profile                            ( UserName )
import Framework.Location.Internal.Types.Chat       ( ChatHolder )
import Framework.Location.Internal.Types.Lobby      ( LobbyId )
import Framework.Location.Internal.Types.Game ( GameId )

data GameView = GameView
    { gameId        :: GameId
    , lobbyId       :: LobbyId
    , chats         :: ChatHolder
    , members       :: [UserName]
    } deriving (Ord, Eq, Read, Show)

