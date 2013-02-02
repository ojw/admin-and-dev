{-# LANGUAGE RecordWildCards, MultiParamTypeClasses #-} 

module Framework.Location.Internal.Views.LocationView where

import Data.Text                                ( Text )
import Framework.Profile                        ( UserName )
import Framework.Location.Internal.Views.LobbyView
import Framework.Location.Internal.Views.MatchmakerView
import Framework.Location.Internal.Views.GameView
import Framework.Location.Internal.Classes.View

data LocationView
    = LVLobbyView LobbyView
    | LVMatchmakerView MatchmakerView
    | LVGameView GameView
