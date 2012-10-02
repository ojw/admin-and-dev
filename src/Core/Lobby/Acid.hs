{-# LANGUAGE DeriveDataTypeable, GADTs, TemplateHaskell, GeneralizedNewtypeDeriving,
    OverloadedStrings, StandaloneDeriving, TypeFamilies, ScopedTypeVariables #-}

module Core.Lobby.Acid

where

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
import Core.Room.Api
import Util.HasAcidState
import Util.GetBody

data Location = InLobby | InMatchmaker | InGame
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

data Lobby game = Lobby
    { _roomId       :: RoomId
    , _locations    :: IxSet UserLocation
    , _game         :: game -- doesn't belong here!
    }

initialLobby :: RoomId -> game -> Lobby game
initialLobby roomId game = Lobby roomId empty game

deriving instance Ord game => Ord (Lobby game)
deriving instance Eq game => Eq (Lobby game)
deriving instance Data game => Data (Lobby game)
deriving instance Typeable1 Lobby
deriving instance Read game => Read (Lobby game)
deriving instance Show game => Show (Lobby game)

$(makeLens ''Lobby)
$(deriveSafeCopy 0 'base ''Lobby)

instance Indexable (Lobby game) where
    empty = ixSet [ ixFun $ \lobby -> [ roomId ^$ lobby ]
                  ]

setLocation :: UserId -> Location -> Update (Lobby game) (IxSet UserLocation)
setLocation userId subLocation = locations %= updateIx userId (UserLocation userId subLocation)

getLocation :: UserId -> Query (Lobby game) Location
getLocation userId = 
    do  lobby <- ask
        case getOne $ (locations ^$ lobby) @= userId of
            Nothing -> return InLobby
            Just l  -> return $ subLocation ^$ l

getLobby :: Query (Lobby game) (Lobby game)
getLobby = ask

getRoomId :: Query (Lobby game) RoomId
getRoomId = 
    do  lobby <- ask
        return $ roomId ^$ lobby

$(makeAcidic ''Lobby ['setLocation, 'getLocation, 'getLobby, 'getRoomId])
