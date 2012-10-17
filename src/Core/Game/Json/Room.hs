{-# LANGUAGE DeriveDataTypeable, FlexibleContexts, GeneralizedNewtypeDeriving
  , MultiParamTypeClasses, OverloadedStrings, ScopedTypeVariables
  , TemplateHaskell, TypeFamilies, FlexibleInstances, RecordWildCards
  , TypeOperators #-}

module Core.Game.Json.Room

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

import Core.Auth.Auth               ( UserId(..) )
import Core.Game.Acid.Types.Room    ( Room(..), RoomId(..), Chat(..) )

instance ToJSON Chat where
    toJSON (Chat ((UserId sender), message)) = object [ "sender" .= sender, "message" .= message ]

instance ToJSON Room where
    toJSON Room{..} = object [ "id" .= _unRoomId _roomId ] 
