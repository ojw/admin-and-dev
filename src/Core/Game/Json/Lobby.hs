{-# LANGUAGE DeriveDataTypeable, FlexibleContexts, GeneralizedNewtypeDeriving
  , MultiParamTypeClasses, OverloadedStrings, ScopedTypeVariables
  , TemplateHaskell, TypeFamilies, FlexibleInstances, RecordWildCards
  , TypeOperators #-}

module Core.Game.Json.Lobby

( encode
, displayLobby
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

instance ToJSON LobbyDisplay where
    toJSON lobby = object [ "id" .= (_unLobbyId $ _LobbyId lobby)
                          , "members" .= map unUserId (_members lobby)
                          ]

data LobbyDisplay = LobbyDisplay
    { _LobbyId      :: LobbyId
    , _members      :: [UserId]
    , _name         :: Text
    }

displayLobby 
    :: (Typeable o , Typeable s , Typeable p,SafeCopy o , SafeCopy s , SafeCopy p, MonadIO m) 
    => AcidState (GameAcid p s o) -> LobbyId -> m (Maybe LobbyDisplay)
displayLobby gameAcid lobbyId = do
    members <- query' gameAcid (GetLobbyMemberIds lobbyId)
    name <- query' gameAcid (GetLobbyName lobbyId)
    return $ LobbyDisplay lobbyId members <$> name
