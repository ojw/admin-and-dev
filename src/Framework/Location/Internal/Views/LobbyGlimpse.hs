{-# LANGUAGE RecordWildCards, MultiParamTypeClasses #-}

module Framework.Location.Internal.Views.LobbyGlimpse where

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

data LobbyGlimpse = LobbyGlimpse
    { name              :: Text
    , description       :: Text
    , lobbyId           :: LobbyId
    , memberCount       :: Int
    , matchmakerCount   :: Int
    } deriving (Ord, Eq, Read, Show) 

instance View Lobby LobbyGlimpse LocationAction where
    view lobby@Lobby{..} = do
        memberCount <- fmap length $ getUsers $ InLobby _lobbyId
        matchmakerIx <- _matchmakers <$> gets _matchmakerState
        let matchmakerCount = length $ toList $ matchmakerIx @= _lobbyId
        return $ LobbyGlimpse
            { name = _name
            , description = _description
            , lobbyId = _lobbyId
            , memberCount = memberCount
            , matchmakerCount = matchmakerCount
            }
