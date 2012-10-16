{-# LANGUAGE DeriveDataTypeable, GADTs, TemplateHaskell, GeneralizedNewtypeDeriving,
    OverloadedStrings, StandaloneDeriving, TypeFamilies, ScopedTypeVariables #-}

module Core.Game.Acid.Types.Location where

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
import Core.Game.Acid.Types.Lobby
import Core.Game.Acid.Types.Matchmaker
import Core.Game.Acid.Types.Game

data Location = InLobby LobbyId | InMatchmaker MatchmakerId | InGame GameId
    deriving (Ord, Eq, Read, Show, Data, Typeable)

$(deriveSafeCopy 0 'base ''Location)

data UserLocation = UserLocation
    { _userId   :: UserId
    , _location :: Maybe Location
    } deriving (Ord, Eq, Read, Show, Data, Typeable)

$(makeLens ''UserLocation)
$(deriveSafeCopy 0 'base ''UserLocation)

instance Indexable UserLocation where
    empty = ixSet [ ixFun $ \location -> [ userId ^$ location ]
                  , ixFun $ \location -> [ _location location ]
                  ]

newtype LocationState = LocationState { _locations :: IxSet UserLocation }
    deriving (Ord, Eq, Read, Show, Data, Typeable, SafeCopy)

$(makeLens ''LocationState)

getLocation :: UserId -> LocationState -> Maybe Location
getLocation userId locationState =
    case getOne $ (locations ^$ locationState) @= userId of
        Nothing         -> Nothing
        Just userLocation  -> location ^$ userLocation

setLocation :: UserId -> Maybe Location -> LocationState -> LocationState
setLocation userId mLocation locationState =
    locations ^%= updateIx userId (UserLocation userId mLocation) $ locationState
