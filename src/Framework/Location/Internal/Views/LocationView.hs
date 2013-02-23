{-# LANGUAGE RecordWildCards, MultiParamTypeClasses #-} 

module Framework.Location.Internal.Views.LocationView where

import Control.Monad.Error
import Framework.Location.Internal.Views.LobbyView
import Framework.Location.Internal.Views.MatchmakerView
import Framework.Location.Internal.Views.GameView
import Framework.Location.Internal.Types.Location
-- import Framework.Location.Internal.Classes.View ( View(..) )
import Framework.Common.Classes ( View(..) )

data LocationView
    = LVLobbyView LobbyView
    | LVMatchmakerView MatchmakerView
    | LVGameView GameView

instance View Location LocationView LocationAction where
    view (LocLobby lobby) = view lobby >>= return . LVLobbyView
    view (LocMatchmaker matchmaker) = view matchmaker >>= return . LVMatchmakerView
    view (LocGame game) = view game >>= return . LVGameView

instance View LocationId LocationView LocationAction where
    view locationId = getLocation locationId >>= maybe (throwError LocationDoesNotExist) view
