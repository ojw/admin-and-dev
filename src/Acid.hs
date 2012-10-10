{-# LANGUAGE DeriveDataTypeable, FlexibleContexts, GeneralizedNewtypeDeriving
  , MultiParamTypeClasses, OverloadedStrings, ScopedTypeVariables
  , TemplateHaskell, TypeFamilies, FlexibleInstances, RecordWildCards
  , TypeOperators, StandaloneDeriving #-}

module Acid 

( Acid(..)
, withAcid
, Games(..)
)

where

import Data.Maybe                   ( fromMaybe )
import Data.Acid                    ( AcidState )
import Data.Acid.Local              ( createCheckpointAndClose 
                                    , openLocalStateFrom)
import Data.SafeCopy
import Data.Data
import System.FilePath              ( (</>) )
import Control.Exception            ( bracket )

import Util.HasAcidState
import Core.Auth.Acid
import Core.Room.Acid            ( RoomState, initialRoomState, RoomId(..) )
import Core.Location.Acid
import Core.Lobby.Acid
import Core.Matchmaker.Acid
import Core.Game.Acid

data Games = Dummy

deriving instance Typeable Games
deriving instance Eq Games
deriving instance Ord Games
$(deriveSafeCopy 0 'base ''Games)

data Acid = Acid
    { acidAuth          :: AcidState AuthState
    , acidProfile       :: AcidState ProfileState
    , acidRoom          :: AcidState RoomState  
    , acidLocation      :: AcidState (LocationState Games)
    , acidLobby         :: AcidState LobbyState
    , acidMatchmaker    :: AcidState MatchmakerState
    , acidGame          :: AcidState Game
    }

withAcid :: Maybe FilePath -- ^ state directory
         -> (Acid -> IO a) -- ^ action
         -> IO a
withAcid mBasePath f =
    let basePath = fromMaybe ".state" mBasePath in
    bracket (openLocalStateFrom (basePath </> "auth")       initialAuthState)       (createCheckpointAndClose) $ \auth ->
    bracket (openLocalStateFrom (basePath </> "profile")    initialProfileState)    (createCheckpointAndClose) $ \profile ->
    bracket (openLocalStateFrom (basePath </> "room")       initialRoomState)       (createCheckpointAndClose) $ \room ->
    bracket (openLocalStateFrom (basePath </> "location")   initialLocationState)   (createCheckpointAndClose) $ \location ->
    bracket (openLocalStateFrom (basePath </> "lobby")      initialLobbyState)      (createCheckpointAndClose) $ \lobby ->
    bracket (openLocalStateFrom (basePath </> "matchmaker") initialMatchmakerState) (createCheckpointAndClose) $ \matchmaker ->
    bracket (openLocalStateFrom (basePath </> "game")       initialGame)            (createCheckpointAndClose) $ \game ->
        f (Acid auth profile room location lobby matchmaker game)
