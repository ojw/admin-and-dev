{-# LANGUAGE RecordWildCards, MultiParamTypeClasses #-}

module Framework.Location.Instances.View.LobbyView where

import Data.Maybe                               ( catMaybes )
import Control.Monad.State                      ( gets )
import Data.IxSet                               ( toList, (@=) )
import Data.Text                                ( Text )
import Data.Functor                             ( (<$>) )
import Control.Lens hiding ( view )

import Framework.Profile                        ( UserId, UserName, lookupUserName )
import Common.Location.Types
import Common.Location.View
import DB.Location.LocationAction
import Framework.Location.Instances.View.MatchmakerView
import Common.Classes ( View(..) )

instance View Lobby LobbyView LocationAction where
    view lobby = do
        memberIds <- getUsers $ InLobby $ lobby ^. lobbyId
        members <- catMaybes <$> mapM lookupUserName memberIds
        matchmakerIx <- getMatchmakers
        let matchmakerList = toList $ matchmakerIx @= (lobby ^. lobbyId)
        matchmakerGlimpses <- mapM view matchmakerList
        return $ LobbyView
            { lvName = lobby ^. name
            , lvDescription = lobby ^. description
            , lvLobbyId = lobby ^. lobbyId
            , lvChats = lobby ^. chats
            , lvMembers = members
            , lvMatchmakers = matchmakerGlimpses
            }

instance View Lobby LobbyGlimpse LocationAction where
    view lobby = do
        memberCount <- fmap length $ getUsers $ InLobby $ lobby ^. lobbyId
        matchmakerIx <- getMatchmakers
        let matchmakerCount = length $ toList $ matchmakerIx @= (lobby ^. lobbyId)
        return $ LobbyGlimpse
            { lgName = lobby ^. name
            , lgDescription = lobby ^. description
            , lgLobbyId = lobby ^. lobbyId
            , lgMemberCount = memberCount
            , lgMatchmakerCount = matchmakerCount
            }
