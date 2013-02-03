

module Framework.Location.Internal.Api where

import Data.Text
import Control.Monad.Reader hiding ( join )

import Framework.Profile as Profile
import Framework.Location.Internal.Types.Location
import Framework.Location.Internal.Instances.Location
import Framework.Location.Internal.Classes.Location
import Framework.Location.Internal.Classes.View
import Framework.Location.Internal.Views.LocationView

-- removed UserId since these will run with MonadReader Profile m
data LocationApi
    = Join LocationId
    | Leave 
    | Look LocationId -- will include data previously requested with ReceiveChat
    | Chat Text LocationId
    | Create LocationId -- will probably patern match on LocationId to determine type of location, ignore id
    | Delete LocationId

join :: (MonadLocationAction m) => LocationId -> m LocationView
join locationId = do
    userId <- currentUserId
    oldLocationId <- getUserLocation userId
    setLocation locationId userId
    onLeave oldLocationId
    onJoin locationId 
    view locationId

tryJoin :: (MonadLocationAction m) => LocationId -> m LocationView
tryJoin locationId = do
    userId <- currentUserId
    oldLocationId <- getUserLocation userId
    canLeave <- canLeave oldLocationId
    canJoin <- canJoin locationId
    if canJoin && canLeave then join locationId else view oldLocationId

tryLeave :: (MonadLocationAction m) => m LocationView
tryLeave = do
    userId <- currentUserId
    currentLocationId <- getUserLocation userId
    exit <- exit currentLocationId
    tryJoin exit

leave :: (MonadLocationAction m) => m LocationView
leave = do
    userId <- currentUserId
    locationId <- getUserLocation userId
    join locationId

-- need to decide on return type
runLocationApi :: (MonadLocationAction m) => LocationApi -> m LocationView
runLocationApi (Join locationId) = tryJoin locationId
runLocationApi Leave = tryLeave
--runLocationApi _ = return ()
