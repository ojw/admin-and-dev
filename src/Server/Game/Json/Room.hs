{-# LANGUAGE DeriveDataTypeable, FlexibleContexts, GeneralizedNewtypeDeriving
  , MultiParamTypeClasses, OverloadedStrings, ScopedTypeVariables
  , TemplateHaskell, TypeFamilies, FlexibleInstances, RecordWildCards
  , TypeOperators #-}

module Server.Game.Json.Room

( encode
)

where

import Prelude hiding ( (.) )
import Control.Category ( (.) )
import Control.Monad.Trans
import Data.Data
import Data.Acid
import Data.Acid.Advanced
import Data.Aeson
import Data.Text as Text

import Server.Profile.Acid            ( UserName(..) )
import Server.Auth.Auth               ( UserId(..) )
import Server.Game.Acid.Types.Room

instance ToJSON Chat where
    toJSON (Chat ((UserName sender), message)) = object [ "sender" .= sender, "message" .= message ]

instance ToJSON ChatList where
    toJSON (ChatList chats) = object [ "chat" .= chats ]
