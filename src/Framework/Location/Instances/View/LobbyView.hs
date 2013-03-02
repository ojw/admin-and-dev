{-# LANGUAGE RecordWildCards, MultiParamTypeClasses #-}

module Framework.Location.Instances.View.LobbyView where

import Data.Maybe                               ( catMaybes )
import Control.Monad.State                      ( gets )
import Data.IxSet                               ( toList, (@=) )
import Data.Text                                ( Text )
import Data.Functor                             ( (<$>) )
import Control.Lens hiding ( view )

import Framework.Profile                        ( UserId, UserName, lookupUserName )
import Framework.Location.Types
import Framework.Location.LocationAction
import Framework.Location.Instances.View.MatchmakerView
import Framework.Common.Classes ( View(..) )

data LobbyView = LobbyView
    { lvName          :: Text
    , lvDescription   :: Text
    , lvLobbyId       :: LobbyId
    , lvChats         :: ChatHolder
    , lvMembers       :: [UserName]
    , lvMatchmakers   :: [MatchmakerGlimpse]
    } deriving (Ord, Eq, Read, Show) 

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

data LobbyGlimpse = LobbyGlimpse
    { lgName              :: Text
    , lgDescription       :: Text
    , lgLobbyId           :: LobbyId
    , lgMemberCount       :: Int
    , lgMatchmakerCount   :: Int
    } deriving (Ord, Eq, Read, Show) 

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
