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
import Framework.Location.Internal.Classes.View ( View(..) )

simplifyMatchmaker :: MonadLocationAction m => Matchmaker -> m (MatchmakerId, (Int, Int), Int, UserId)
simplifyMatchmaker Matchmaker{..} = do
    memberCount <- length <$> getUsers (InMatchmaker _matchmakerId)
    return (_matchmakerId, _capacity, memberCount, _owner)

data LobbyView = LobbyView
    { name          :: Text
    , description   :: Text
    , lobbyId       :: LobbyId
    , chats         :: ChatHolder
    , members       :: [UserName]
    , matchmakers   :: [(MatchmakerId, (Int, Int), Int, UserId)]
    } deriving (Ord, Eq, Read, Show) 

instance View Lobby LobbyView where
    view lobby@Lobby{..} = do
        memberIds <- getUsers $ InLobby _lobbyId
        members <- catMaybes <$> mapM lookupUserName memberIds
        matchmakerIx <- _matchmakers <$> gets _matchmakerState
        matchmakers <- mapM simplifyMatchmaker $ toList $ matchmakerIx @= _lobbyId
        return $ LobbyView
            { name = _name
            , description = _description
            , lobbyId = _lobbyId
            , chats = _chats
            , members = members
            , matchmakers = matchmakers
            }
