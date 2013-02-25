{-# LANGUAGE RecordWildCards, MultiParamTypeClasses #-}

module Framework.Location.Internal.Views.LobbyView where

import Data.Maybe                               ( catMaybes )
import Control.Monad.State                      ( gets )
import Data.IxSet                               ( toList, (@=) )
import Data.Text                                ( Text )
import Data.Functor                             ( (<$>) )

import Framework.Profile                        ( UserId, UserName, lookupUserName )
import Framework.Location.Internal.Types.Lobby  ( Lobby(..), LobbyId )
import Framework.Location.Internal.Types.Chat   ( ChatHolder )
import Framework.Location.Internal.Types.Matchmaker
import Framework.Location.Internal.Types.Location
import Framework.Location.Internal.Views.MatchmakerGlimpse
import Framework.Common.Classes ( View(..) )

data LobbyView = LobbyView
    { name          :: Text
    , description   :: Text
    , lobbyId       :: LobbyId
    , chats         :: ChatHolder
    , members       :: [UserName]
    , matchmakers   :: [MatchmakerGlimpse]
    } deriving (Ord, Eq, Read, Show) 

instance View Lobby LobbyView LocationAction where
    view lobby@Lobby{..} = do
        memberIds <- getUsers $ InLobby _lobbyId
        members <- catMaybes <$> mapM lookupUserName memberIds
        matchmakerIx <- _matchmakers <$> gets _matchmakerState
        let matchmakerList = toList $ matchmakerIx @= _lobbyId
        matchmakerGlimpses <- mapM view matchmakerList
        return $ LobbyView
            { name = _name
            , description = _description
            , lobbyId = _lobbyId
            , chats = _chats
            , members = members
            , matchmakers = matchmakerGlimpses
            }
