{-# LANGUAGE DatatypeContexts #-}

module Plugins.Lobby.Acid

where

import Data.IxSet               ( IxSet )
import Data.Acid

import Plugins.Auth.Acid        ( UserId )
import Plugins.Room.Acid.Core   ( RoomId )
import Plugins.Profile.Acid
-- import Util.HasAcidState

-- state is type of game state to be stored during gameplay
-- options is type of all options (including players joining game) to store before game starts
-- permissions is datatype that game will translate to bool for players trying to join lobby
-- possibly have OngoingGame and OpenGame types similarly parameterized in Plugins.Game for use here
--
-- might need to have class constraint on a parameter! this might be getting tricky...
--
data (Permissions permissions, Options options) => Lobby state options permissions = Lobby
    { users         :: [UserId]
    , games         :: IxSet state
    , room          :: RoomId
    , openGame      :: IxSet options
    , permissions   :: permissions
    }

-- will go in Game
class Permissions p where
    canJoin :: UserId -> p -> Query ProfileState Bool

-- also belongs in Game
class Options o where
-- not sure what goes here yet
