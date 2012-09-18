{-# LANGUAGE DeriveDataTypeable, FlexibleContexts, GeneralizedNewtypeDeriving
  , MultiParamTypeClasses, OverloadedStrings, ScopedTypeVariables
  , TemplateHaskell, TypeFamilies, FlexibleInstances, RecordWildCards
  , TypeOperators #-}

module Plugins.Room 

( RoomState(..) -- state to store in Acid
, initialRoomState
, roomAPISite   -- Site to implsite in API
) 

where

import Plugins.Room.Acid
import Plugins.Room.Api
