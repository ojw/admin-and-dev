{-# LANGUAGE RecordWildCards, MultiParamTypeClasses #-}

module Framework.Location.Internal.Views.LobbyView where

import Data.Maybe                               ( catMaybes )
import Data.Text                                ( Text )
import Data.Functor                             ( (<$>) )

import Framework.Profile                        ( UserId, UserName, lookupUserName )
import Framework.Location.Internal.Types.Lobby  ( Lobby(..), LobbyId )
import Framework.Location.Internal.Types.Chat   ( ChatHolder )
import Framework.Location.Internal.Types.Location
import Framework.Location.Internal.Classes.View

data LobbyView = LobbyView
    { name         :: Text
    , description  :: Text
    , lobbyId      :: LobbyId
    , chats        :: ChatHolder
    , members      :: [UserName]
    } deriving (Ord, Eq, Read, Show) 

instance View Lobby LobbyView where
    view lobby@Lobby{..} = do
        memberIds <- getUsers $ InLobby _lobbyId
        members <- catMaybes <$> mapM lookupUserName memberIds
        return $ LobbyView
            { name = _name
            , description = _description
            , lobbyId = _lobbyId
            , chats = _chats
            , members = members
            }
