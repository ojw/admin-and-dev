{-# LANGUAGE RecordWildCards, MultiParamTypeClasses #-}

module Framework.Location.Instances.View.MatchmakerGlimpse where

import Control.Monad.Error
import Data.Maybe                                   ( catMaybes, fromJust )
import Data.Functor                                 ( (<$>) )
import Data.Text                                    ( Text )
import Control.Lens

import Framework.Profile                            ( UserName, lookupUserName )
import Framework.Location.Types
import Framework.Location.LocationAction
import Framework.Common.Classes ( View(..) )

data MatchmakerGlimpse = MatchmakerGlimpse
    { mgMatchmakerId  :: MatchmakerId
    , mgCapacity      :: (Int, Int) -- (min, max)
    , mgOwner         :: UserName
    , mgMembers       :: Int
    } deriving (Ord, Eq, Read, Show)

-- Probably should have used case instead of maybe... this looks a li'l silly.
instance View Matchmaker MatchmakerGlimpse LocationAction where
    view matchmaker = do
        mOwnerName <- lookupUserName (matchmaker ^. owner)
        members <- fmap length $ getUsers $ InMatchmaker $ matchmaker ^. matchmakerId
        maybe (throwError OtherLocationError)   (\ownerName -> return $ MatchmakerGlimpse
                { mgMatchmakerId = matchmaker ^. matchmakerId
                , mgCapacity = matchmaker ^. capacity
                , mgOwner = ownerName
                , mgMembers = members
                }
                                                ) mOwnerName
