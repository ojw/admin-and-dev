{-# LANGUAGE RecordWildCards, MultiParamTypeClasses #-} 

module Framework.Location.Instances.View.LocationView where

import Control.Monad.Error
import Framework.Location.Instances.View.LobbyView
import Framework.Location.Instances.View.MatchmakerView
import Framework.Location.Instances.View.GameView
import Common.Location.Types
import DB.Location.LocationAction
import Common.Classes ( View(..) )
import Common.Location.View
import Data.Text ( Text )

instance View Location LocationView LocationAction where
    view (LocLobby lobby) = view lobby >>= return . LVLobbyView
    view (LocMatchmaker matchmaker) = view matchmaker >>= return . LVMatchmakerView
    view (LocGame game) = view game >>= return . LVGameView

instance View LocationId LocationView LocationAction where
    view locationId = getLocation locationId >>= maybe (throwError LocationDoesNotExist) view
