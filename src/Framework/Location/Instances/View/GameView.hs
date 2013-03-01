{-# LANGUAGE RecordWildCards, MultiParamTypeClasses #-}

module Framework.Location.Instances.View.GameView where

import Data.Maybe                                   ( catMaybes, fromJust )
import Data.Functor                                 ( (<$>) )
import Data.Text                                    ( Text )
import Control.Lens

import Framework.Location.LocationAction
import Framework.Profile                            ( UserName, lookupUserName )
import Framework.Location.Types
import Framework.Common.Classes ( View(..) )

data GameView = GameView
    { gvGameId        :: GameId
    , gvLobbyId       :: LobbyId
    , gvChats         :: ChatHolder
    , gvMembers       :: [UserName]
    } deriving (Ord, Eq, Read, Show)

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
