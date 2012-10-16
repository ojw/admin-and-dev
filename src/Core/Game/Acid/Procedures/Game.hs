{-# LANGUAGE MultiParamTypeClasses, DeriveDataTypeable, GeneralizedNewtypeDeriving, Rank2Types, StandaloneDeriving #-}

module Core.Game.Acid.Procedures.Game where

import Data.Acid
import Data.IxSet
import Data.SafeCopy    ( SafeCopy )

import Core.Auth.Acid               ( UserId )
import Core.Room.Acid               ( RoomId )
import Core.Game.Acid.Types.Lobby   ( LobbyId)

