{-# LANGUAGE DeriveDataTypeable, FlexibleContexts, GeneralizedNewtypeDeriving
  , MultiParamTypeClasses, OverloadedStrings, ScopedTypeVariables
  , TemplateHaskell, TypeFamilies, FlexibleInstances, RecordWildCards
  , TypeOperators #-}

module Plugins.Room.Acid.Json

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

import Plugins.Auth             ( UserId(..) )
import Plugins.Room.Acid.Core   ( Room(..), RoomId(..), Chat(..) )

instance ToJSON Chat where
    toJSON (Chat ((UserId sender), message)) = object [ "sender" .= sender, "message" .= message ]

instance ToJSON Room where
    toJSON Room{..} = object [ "id" .= _unRoomId _roomId ] 
