{-# LANGUAGE RecordWildCards, MultiParamTypeClasses #-}

module Framework.Location.Instances.Views.LobbyGlimpse where

import Data.Maybe                               ( catMaybes )
import Control.Monad.State                      ( get )
import Data.IxSet                               ( toList, (@=) )
import Data.Text                                ( Text )
import Data.Functor                             ( (<$>) )
import Control.Lens as Lens

import Framework.Profile                        ( UserId, UserName, lookupUserName )
import Framework.Location.Types
import Framework.Location.LocationAction
import Framework.Location.Instances.Views.MatchmakerGlimpse
import Framework.Common.Classes ( View(..) )

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
