

module Framework.Location.Internal.Views.LobbyView where

import Data.Text                                ( Text )
import Framework.Profile                        ( UserName )
import Framework.Location.Internal.Types.Lobby  ( LobbyId )
import Framework.Location.Internal.Types.Chat   ( ChatHolder )

data LobbyView = LobbyView
    { name         :: Text
    , description  :: Text
    , lobbyId      :: LobbyId
    , chats        :: ChatHolder
    , members      :: [UserName]
    } deriving (Ord, Eq, Read, Show) 
