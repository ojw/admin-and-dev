{-# LANGUAGE DeriveDataTypeable, FlexibleContexts, GeneralizedNewtypeDeriving
  , MultiParamTypeClasses, OverloadedStrings, ScopedTypeVariables
  , TemplateHaskell, TypeFamilies, FlexibleInstances, RecordWildCards
  , TypeOperators #-}

module Server.Game.Json.Lobby

( encode
, displayLobby
)

where

import Control.Applicative
import Control.Monad.Reader
import Data.Functor
import Data.Maybe
import Data.Aeson
import Data.Acid hiding ( query )
import Data.Data
import Data.SafeCopy
import Data.Acid.Advanced
import Data.Text as Text hiding ( map )

import Util.HasAcidState
import Server.Profile.Acid
--import Server.Profile.Api
import Server.Auth.Acid
import Server.Game.Acid.GameAcid
import Server.Game.Acid.Types.Matchmaker ( Matchmaker, MatchmakerId(..) )
import Server.Game.Acid.Procedures

instance ToJSON LobbyDisplay where
    toJSON lobby = object [ "id" .= (_unLobbyId $ _lobbyId lobby)
                          , "name" .= (_name lobby)
                          , "members" .= map _unUserName (_members lobby)
                          ]

data LobbyDisplay = LobbyDisplay
    { _lobbyId      :: LobbyId
    , _members      :: [UserName]
    , _name         :: Text
    }

displayLobby 
    :: (Typeable o , Typeable s , Typeable p,SafeCopy o , SafeCopy s , SafeCopy p, MonadIO m, HasAcidState m Server.Profile.Acid.ProfileState, Functor m) 
    => AcidState (GameAcid p s o) -> LobbyId -> m (Maybe LobbyDisplay)
displayLobby gameAcid lobbyId = do
    members <- query' gameAcid (GetLobbyMemberIds lobbyId)
    name <- query' gameAcid (GetLobbyName lobbyId)
    maybeNames <- mapM (\userId -> query (AskName userId)) members
    return $ LobbyDisplay lobbyId (catMaybes maybeNames) <$> name
