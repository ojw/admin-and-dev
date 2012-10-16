

module Core.Game.Acid.Procedures.Location

where

import Core.Auth.Acid        ( UserId )
import Core.Game.Acid.Types.Location
import Core.Game.Acid.Acid

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

getLocation' :: UserId -> LocationState -> Maybe Location
getLocation' userId locationState =
    case getOne $ (locations ^$ locationState) @= userId of
        Nothing         -> Nothing
        Just userLocation  -> location ^$ userLocation

setLocation' :: UserId -> Maybe Location -> LocationState -> LocationState
setLocation' userId mLocation locationState =
    locations ^%= updateIx userId (UserLocation userId mLocation) $ locationState

getLocation :: UserId -> Query (GameAcid p s o) (Maybe Location)
getLocation userId = do
    gameAcid <- ask
    return $ getLocation' userId $ gameAcid ^. locationState

setLocation :: UserId -> Maybe Location -> Update (GameAcid p s o) LocationState
setLocation userId mLocation = do
    locationState %= setLocation' userId mLocation