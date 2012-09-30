{-# LANGUAGE DeriveDataTypeable, FlexibleContexts, GeneralizedNewtypeDeriving
  , MultiParamTypeClasses, OverloadedStrings, ScopedTypeVariables
  , TemplateHaskell, TypeFamilies, FlexibleInstances, RecordWildCards
  , TypeOperators #-}

module Core.Room.Room

( RoomState(..) -- state to store in Acid
, initialRoomState
, roomAPISite   -- Site to implsite in API
) 

where

import Core.Room.Acid
import Core.Room.Api
