{-# LANGUAGE RecordWildCards, MultiParamTypeClasses #-}

module Framework.Location.Instances.View.GameView where

import Data.Maybe                                   ( catMaybes, fromJust )
import Data.Functor                                 ( (<$>) )
import Data.Text                                    ( Text )
import Control.Lens

import DB.Location.LocationAction
import Framework.Profile                            ( UserName, lookupUserName )
import Common.Location.Types
import Common.Location.View
import Common.Classes ( View(..) )

instance View Game GameView LocationAction where
    view game = do
        memberIds <- getUsers $ InGame $ game ^. gameId
        memberNames <- catMaybes <$> mapM lookupUserName memberIds
        return $ GameView
            { gvGameId = game ^. gameId
            , gvLobbyId = game ^. lobbyId
            , gvChats = game ^. chats
            , gvMembers = memberNames
            }
