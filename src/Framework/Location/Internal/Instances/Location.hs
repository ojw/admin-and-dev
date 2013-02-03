{-# LANGUAGE GeneralizedNewtypeDeriving, DeriveDataTypeable, TemplateHaskell, 
    MultiParamTypeClasses, FlexibleContexts, FlexibleInstances #-}

module Framework.Location.Internal.Instances.Location where

import Control.Monad hiding ( join )
import Data.Functor
import Control.Monad.State hiding ( join )
import Control.Monad.Reader hiding ( join )
import Control.Monad.Error hiding ( join )
import Data.SafeCopy
import Data.Data
import Data.Acid
import Data.Lens
import Data.Lens.Template
import Data.IxSet
import Data.Text                            ( Text )

import Framework.Profile ( UserId )
import Framework.Profile as Profile
import Framework.Location.Internal.Instances.Lobby as Lobby
import Framework.Location.Internal.Instances.Matchmaker as Matchmaker
import Framework.Location.Internal.Instances.Matchmaker as Matchmaker
import Framework.Location.Internal.Instances.Game as Game
import Framework.Location.Internal.Types.Chat hiding ( addChat )
import Framework.Location.Internal.Types.Location
import Framework.Location.Internal.Classes.Location

instance Loc Location where
    canJoin (LocLobby lobby) = canJoin lobby
    canJoin (LocMatchmaker matchmaker) = canJoin matchmaker
    canJoin (LocGame game) = canJoin game

    onJoin (LocLobby lobby) = onJoin lobby
    onJoin (LocMatchmaker matchmaker) = onJoin matchmaker
    onJoin (LocGame game) = onJoin game
    
    canLeave (LocLobby lobby) = canLeave lobby
    canLeave (LocMatchmaker matchmaker) = canLeave matchmaker
    canLeave (LocGame game) = canLeave game

    onLeave (LocLobby lobby) = onLeave lobby
    onLeave (LocMatchmaker matchmaker) = onLeave matchmaker
    onLeave (LocGame game) = onLeave game

    exit (LocLobby lobby) = exit lobby
    exit (LocMatchmaker matchmaker) = exit matchmaker
    exit (LocGame game) = exit game

    chat c (LocLobby lobby) = chat c lobby
    chat c (LocMatchmaker matchmaker) = chat c matchmaker
    chat c (LocGame game) = chat c game

instance Loc LocationId where
    canJoin locationId = getLocation locationId >>= maybe (throwError LocationDoesNotExist) canJoin
    onJoin locationId = getLocation locationId >>= maybe (throwError LocationDoesNotExist) onJoin
    canLeave locationId = getLocation locationId >>= maybe (throwError LocationDoesNotExist) canLeave
    onLeave locationId = getLocation locationId >>= maybe (throwError LocationDoesNotExist) onLeave
    exit locationId = getLocation locationId >>= maybe (throwError LocationDoesNotExist) exit
    chat c locationId = getLocation locationId >>= maybe (throwError LocationDoesNotExist) (chat c)
