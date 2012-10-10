{-# LANGUAGE DeriveDataTypeable, GADTs, TemplateHaskell, GeneralizedNewtypeDeriving,
    OverloadedStrings, StandaloneDeriving, TypeFamilies, ScopedTypeVariables #-}

module Core.Game.Acid where

import Control.Applicative hiding (empty)
import Control.Monad.Reader 
import Data.IxSet
import Data.Acid
import Data.SafeCopy
import Data.Data
import Data.Lens
import Data.Lens.Template
import Data.Aeson
import Data.Text hiding (empty)
import Data.ByteString.Lazy as L hiding (empty)
import Happstack.Server

import Core.Auth.Acid        ( UserId )
import Core.Room.Acid        ( RoomId )
import Core.Lobby.Acid
import Core.Matchmaker.Acid
import Core.Room.Api
import Util.HasAcidState
import Util.GetBody

data Location = InLobby LobbyId | InMatchmaker | InGame
    deriving (Ord, Eq, Read, Show, Data, Typeable)

$(deriveSafeCopy 0 'base ''Location)

data UserLocation = UserLocation 
    { _userId        :: UserId
    , _subLocation   :: Location
    } deriving (Ord, Eq, Data, Typeable, Read, Show)

$(makeLens ''UserLocation)
$(deriveSafeCopy 0 'base ''UserLocation)

instance Indexable UserLocation where
    empty = ixSet [ ixFun $ \location -> [ userId ^$ location ]
                  , ixFun $ \location -> [ subLocation ^$ location ]
                  ]

data Game = Game
    { _lobbies      :: IxSet Lobby
    , _locations    :: IxSet UserLocation
    }

-- this does not make sense! This is a temporary hack to get everything hooked up
initialGame :: Game
initialGame = Game empty empty

{-
deriving instance Ord game => Ord (Lobby game)
deriving instance Eq game => Eq (Lobby game)
deriving instance Data game => Data (Lobby game)
deriving instance Typeable1 Lobby
deriving instance Read game => Read (Lobby game)
deriving instance Show game => Show (Lobby game)
-}

$(makeLens ''Game)
$(deriveSafeCopy 0 'base ''Game)

setLocation :: UserId -> Location -> Update Game (IxSet UserLocation)
setLocation userId subLocation = locations %= updateIx userId (UserLocation userId subLocation)

getLocation :: UserId -> Query Game (Maybe Location)
getLocation userId = 
    do  game <- ask
        case getOne $ (locations ^$ game) @= userId of
            -- Nothing -> return InLobby
            Just l  -> return $ Just $ subLocation ^$ l

$(makeAcidic ''Game ['setLocation, 'getLocation])
