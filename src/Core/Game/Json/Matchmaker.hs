{-# LANGUAGE DeriveDataTypeable, FlexibleContexts, GeneralizedNewtypeDeriving
  , MultiParamTypeClasses, OverloadedStrings, ScopedTypeVariables
  , TemplateHaskell, TypeFamilies, FlexibleInstances, RecordWildCards
  , TypeOperators #-}

module Core.Matchmaker.Json

( encode
)

where

import Control.Applicative
import Control.Monad.Reader
import Data.Functor
import Data.Aeson
import Data.Acid
import Data.Data
import Data.SafeCopy
import Data.Acid.Advanced
import Data.Text as Text hiding ( map )

import Core.Auth.Acid
import Core.Game.Acid.GameAcid
import Core.Game.Acid.Types.Matchmaker ( Matchmaker, MatchmakerId(..) )
import Core.Game.Acid.Procedures

instance ToJSON MatchmakerDisplay where
    toJSON matchmaker = object  [ "id" .= (_unMatchmakerId $ _matchmakerId matchmaker)
                                , "owner" .= unUserId (_owner matchmaker)
                                , "members" .= map unUserId (_members matchmaker)
                                , "capacity" .= (_capacity matchmaker)
                                ]

data MatchmakerDisplay = MatchmakerDisplay
    { _matchmakerId :: MatchmakerId
    , _owner        :: UserId
    , _members      :: [UserId]
    , _capacity     :: Int
    }

--displayMatchmaker :: MatchmakerId -> Query (GameAcid p s o) (Maybe MatchmakerDisplay)
displayMatchmaker 
    :: (Typeable o , Typeable s , Typeable p,SafeCopy o , SafeCopy s , SafeCopy p, MonadReader (AcidState (GameAcid p s o)) m, MonadIO m) 
    => MatchmakerId -> m (Maybe MatchmakerDisplay)
displayMatchmaker matchmakerId = do
    gameAcid <- ask
    members <- query' gameAcid (GetMatchmakerMemberIds matchmakerId)
    owner <- query' gameAcid (GetMatchmakerOwner matchmakerId)
    capacity <- query' gameAcid (GetMatchmakerCapacity matchmakerId)
    return $ MatchmakerDisplay matchmakerId <$> owner <*> members <*> capacity
