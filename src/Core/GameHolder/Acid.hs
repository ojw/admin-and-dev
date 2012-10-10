{-# LANGUAGE DeriveDataTypeable, GADTs, TemplateHaskell, GeneralizedNewtypeDeriving,
    OverloadedStrings, StandaloneDeriving, TypeFamilies, ScopedTypeVariables #-}

module Core.GameHolder.Acid where

import Control.Applicative hiding (empty)
import Control.Monad.Reader 
import Data.IxSet
import Data.Acid
import Data.SafeCopy
import Data.Data
import Data.Lens
import Data.Lens.Template
import Data.Text hiding (empty)
import Data.ByteString.Lazy as L hiding (empty)

import Core.Auth.Acid        ( UserId )
import Core.Lobby.Acid
import Core.Matchmaker.Acid

data Location = InLobby LobbyId | InMatchmaker | InGame
    deriving (Ord, Eq, Read, Show, Data, Typeable)

$(deriveSafeCopy 0 'base ''Location)

data UserLocation = UserLocation 
    { _userId       :: UserId
    , _location     :: Location
    } deriving (Ord, Eq, Data, Typeable, Read, Show)

$(makeLens ''UserLocation)
$(deriveSafeCopy 0 'base ''UserLocation)

instance Indexable UserLocation where
    empty = ixSet [ ixFun $ \loc -> [ userId ^$ loc ]
                  , ixFun $ \loc -> [ location ^$ loc ]
                  ]

data GameHolder = GameHolder
    { _lobbies      :: LobbyState
    , _defaultLobby :: Maybe LobbyId -- temporarily Maybe while I decide how to initialize these
    , _matchmakers  :: MatchmakerState
    , _locations    :: IxSet UserLocation
    }

-- this does not make sense! This is a temporary hack to get everything hooked up
initialGameHolder :: GameHolder
initialGameHolder = GameHolder initialLobbyState Nothing initialMatchmakerState empty

{-
deriving instance Ord game => Ord (Lobby game)
deriving instance Eq game => Eq (Lobby game)
deriving instance Data game => Data (Lobby game)
deriving instance Typeable1 Lobby
deriving instance Read game => Read (Lobby game)
deriving instance Show game => Show (Lobby game)
-}

$(makeLens ''GameHolder)
$(deriveSafeCopy 0 'base ''GameHolder)

setLocation :: UserId -> Location -> Update GameHolder (IxSet UserLocation)
setLocation userId subLocation = locations %= updateIx userId (UserLocation userId subLocation)

getLocation :: UserId -> Query GameHolder (Maybe Location)
getLocation userId = 
    do  game <- ask
        case getOne $ (locations ^$ game) @= userId of
            Nothing -> return $ InLobby <$> (defaultLobby ^$ game)
            Just l  -> return $ Just $ location ^$ l

$(makeAcidic ''GameHolder ['setLocation, 'getLocation])
