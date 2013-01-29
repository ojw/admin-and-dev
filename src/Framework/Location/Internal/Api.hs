

module Framework.Location.Internal.Api where

import Data.Text
import Control.Monad.Reader hiding ( join )

import Framework.Profile.Profile as Profile
import Framework.Location.Internal.Types.Location
import Framework.Location.Internal.Instances.Location
import Framework.Location.Internal.Classes.Location

-- removed UserId since these will run with MonadReader Profile m
data LocationApi
    = Join LocationId
    | Leave 
    | Look LocationId -- will include data previously requested with ReceiveChat
    | Chat Text LocationId
    | Create LocationId -- will probably patern match on LocationId to determine type of location, ignore id
    | Delete LocationId

join :: (LocationAction p m) => LocationId -> m ()
join locationId = do
    userId <- asks Profile.userId
    oldLocationId <- getUserLocation userId
    setLocation locationId userId
    onLeave oldLocationId
    onJoin locationId 

tryJoin :: (LocationAction p m) => LocationId -> m ()
tryJoin locationId = do
    userId <- asks Profile.userId
    oldLocationId <- getUserLocation userId
    canLeave <- canLeave oldLocationId
    canJoin <- canJoin locationId
    if canJoin && canLeave then join locationId else return ()

tryLeave :: (LocationAction p m) => m ()
tryLeave = do
    userId <- asks Profile.userId
    currentLocationId <- getUserLocation userId
    exit <- exit currentLocationId
    tryJoin exit

leave :: (LocationAction p m) => m ()
leave = do
    userId <- asks Profile.userId
    locationId <- getUserLocation userId
    join locationId

-- need to decide on return type
runLocationApi :: (LocationAction p m) => LocationApi -> m ()
runLocationApi (Join locationId) = tryJoin locationId
runLocationApi Leave = tryLeave
runLocationApi _ = return ()
