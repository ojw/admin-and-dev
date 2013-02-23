{-# LANGUAGE RecordWildCards, MultiParamTypeClasses #-}

module Framework.Location.Internal.Views.GameView where

import Data.Maybe                                   ( catMaybes, fromJust )
import Data.Functor                                 ( (<$>) )
import Data.Text                                    ( Text )
import Framework.Profile                            ( UserName, lookupUserName )
import Framework.Location.Internal.Types.Chat       ( ChatHolder )
import Framework.Location.Internal.Types.Lobby      ( LobbyId )
import Framework.Location.Internal.Types.Game       ( GameId, Game(..) )
import Framework.Location.Internal.Types.Location
import Framework.Common.Classes ( View(..) )

data GameView = GameView
    { gameId        :: GameId
    , lobbyId       :: LobbyId
    , chats         :: ChatHolder
    , members       :: [UserName]
    } deriving (Ord, Eq, Read, Show)

instance View Game GameView LocationAction where
    view game@Game{..} = do
        memberIds <- getUsers $ InGame _gameId
        memberNames <- catMaybes <$> mapM lookupUserName memberIds
        return $ GameView
            { gameId = _gameId
            , lobbyId = _lobbyId
            , chats = _chats
            , members = memberNames
            }
