{-# LANGUAGE DeriveDataTypeable, FlexibleContexts, GeneralizedNewtypeDeriving
  , MultiParamTypeClasses, OverloadedStrings, ScopedTypeVariables
  , TemplateHaskell, TypeFamilies, FlexibleInstances, RecordWildCards
  , TypeOperators #-}

module Server.Game.Json.Matchmaker

( encode
, displayMatchmaker
)

where

import Control.Applicative
import Control.Monad.Reader
import Data.Maybe
import Data.Functor
import Data.Aeson
import Data.Acid hiding ( query ) 
import Data.Data
import Data.SafeCopy
import Data.Acid.Advanced
import Data.Text as Text hiding ( map )

import Util.HasAcidState
import Server.Profile.Acid
import Server.Auth.Acid
import Server.Game.Acid.GameAcid
import Server.Game.Acid.Types.Matchmaker ( Matchmaker, MatchmakerId(..) )
import Server.Game.Acid.Procedures

instance ToJSON MatchmakerDisplay where
    toJSON matchmaker = object  [ "id" .= (_unMatchmakerId $ _matchmakerId matchmaker)
                                , "owner" .= unUserId (_owner matchmaker)
                                , "members" .= map _unUserName (_members matchmaker)
                                , "capacity" .= (_capacity matchmaker)
                                ]

data MatchmakerDisplay = MatchmakerDisplay
    { _matchmakerId :: MatchmakerId
    , _owner        :: UserId
    , _members      :: [UserName]
    , _capacity     :: Int
    }

displayMatchmaker 
    :: (Typeable o , Typeable s , Typeable p,SafeCopy o , SafeCopy s , SafeCopy p, MonadIO m, HasAcidState m Server.Profile.Acid.ProfileState, Functor m)
    => AcidState (GameAcid p s o) -> MatchmakerId -> m (Maybe MatchmakerDisplay)
displayMatchmaker gameAcid matchmakerId = do
    members <- query' gameAcid (GetMatchmakerMemberIds matchmakerId)
    memberNames <- mapM (\userId -> query (AskName userId)) (fromJust members) -- DANGER! DANGER!
    owner <- query' gameAcid (GetMatchmakerOwner matchmakerId)
    capacity <- query' gameAcid (GetMatchmakerCapacity matchmakerId)
    return $ MatchmakerDisplay matchmakerId <$> owner <*> Just (catMaybes memberNames)  <*> capacity
