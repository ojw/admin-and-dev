{-# LANGUAGE MultiParamTypeClasses, DeriveDataTypeable, GeneralizedNewtypeDeriving, Rank2Types, StandaloneDeriving, FlexibleContexts #-}

module Core.Game.Acid.Procedures.Game where

import Data.Acid
import Data.IxSet
import Data.SafeCopy    ( SafeCopy )

import Core.Auth.Acid               ( UserId )
import Core.Room.Acid               ( RoomId )
import Core.Game.Acid.Types.Lobby   ( LobbyId)
import Core.Game.Acid.GameAcid

dummy :: (SafeCopy (GameState p s), SafeCopy o) => Update (GameAcid p s o) ()
dummy = return ()
