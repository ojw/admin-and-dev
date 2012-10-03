{-# LANGUAGE DeriveDataTypeable, FlexibleContexts, GeneralizedNewtypeDeriving
  , MultiParamTypeClasses, OverloadedStrings, ScopedTypeVariables
  , TemplateHaskell, TypeFamilies, FlexibleInstances, RecordWildCards
  , TypeOperators #-}

module Core.Matchmaker.Json

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

import Core.Auth.Auth           ( UserId(..) )
import Core.Matchmaker.Acid

instance ToJSON Matchmaker where
    toJSON matchmaker = object  [ "id" .= _unMatchmakerId (_matchmakerId matchmaker)
                                , "available" .= (availableCapacity matchmaker) 
                                , "capacity" .= _capacity matchmaker
                                ]
