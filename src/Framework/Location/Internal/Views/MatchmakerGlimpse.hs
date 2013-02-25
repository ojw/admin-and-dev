{-# LANGUAGE RecordWildCards, MultiParamTypeClasses #-}

module Framework.Location.Internal.Views.MatchmakerGlimpse where

import Control.Monad.Error
import Data.Maybe                                   ( catMaybes, fromJust )
import Data.Functor                                 ( (<$>) )
import Data.Text                                    ( Text )
import Framework.Profile                            ( UserName, lookupUserName )
import Framework.Location.Internal.Types.Chat       ( ChatHolder )
import Framework.Location.Internal.Types.Lobby      ( LobbyId )
import Framework.Location.Internal.Types.Matchmaker ( MatchmakerId, Matchmaker(..) )
import Framework.Location.Internal.Types.Location
import Framework.Common.Classes ( View(..) )

data MatchmakerGlimpse = MatchmakerGlimpse
    { matchmakerId  :: MatchmakerId
    , capacity      :: (Int, Int) -- (min, max)
    , owner         :: UserName
    , members       :: Int
    } deriving (Ord, Eq, Read, Show)

-- Probably should have used case instead of maybe... this looks a li'l silly.
instance View Matchmaker MatchmakerGlimpse LocationAction where
    view matchmaker@Matchmaker{..} = do
        mOwnerName <- lookupUserName _owner
        members <- fmap length $ getUsers $ InMatchmaker _matchmakerId
        maybe (throwError OtherLocationError)   (\ownerName -> return $ MatchmakerGlimpse
                { matchmakerId = _matchmakerId
                , capacity = _capacity
                , owner = ownerName
                , members = members
                }
                                                ) mOwnerName
