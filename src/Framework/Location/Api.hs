module Framework.Location.Api where

import Data.Text
import Control.Monad.Reader hiding ( join )
import Control.Monad.Error ( throwError )
import Control.Monad.State ( get )

import Framework.Profile as Profile
import Framework.Location.Internal.Types.Location
import Framework.Location.Internal.Instances.Location
import Framework.Location.Internal.Classes.Location hiding ( chat )
import Framework.Location.Internal.Views.LocationView
import Framework.Common.Classes as Classes ( view, create, delete', add' )
import qualified Framework.Location.Internal.Classes.Location as Location
import Data.Text ( Text, pack )

-- removed UserId since these will run with MonadReader Profile m
data LocationApi
    = Join LocationId
    | Leave 
    | Look LocationId -- will include data previously requested with ReceiveChat
    | Chat Text LocationId
    | Create LocationOptions -- will probably patern match on LocationId to determine type of location, ignore id
    | Delete LocationId

join :: LocationId -> LocationAction LocationView
join locationId = do
    userId <- currentUserId
    oldLocationId <- getUserLocation userId
    setLocation locationId userId
    onLeave oldLocationId
    onJoin locationId 
    view locationId

tryJoin :: LocationId -> LocationAction LocationView
tryJoin locationId = do
    userId <- currentUserId
    oldLocationId <- getUserLocation userId
    canLeave <- canLeave oldLocationId
    canJoin <- canJoin locationId
    if canJoin && canLeave then join locationId else view oldLocationId

tryLeave :: LocationAction LocationView
tryLeave = do
    userId <- currentUserId
    currentLocationId <- getUserLocation userId
    exit <- exit currentLocationId
    tryJoin exit

leave :: LocationAction LocationView
leave = do
    userId <- currentUserId
    locationId <- getUserLocation userId
    join locationId

chat :: Text -> LocationId -> LocationAction LocationView
chat text locationId = do
    userName <- currentUserName
    Location.chat (userName, text) locationId
    return $ LVMessage $ pack "Chat added."
    view locationId

create :: LocationOptions -> LocationAction LocationView
create locationOptions = do
    add' $ Classes.create locationOptions
    return $ LVMessage $ pack "Added."

delete :: LocationId -> LocationAction LocationView
delete locationId = do
    delete' locationId
    return $ LVMessage $ pack "Deleted."

runLocationApi :: LocationApi -> LocationAction LocationView
runLocationApi (Join locationId) = tryJoin locationId
runLocationApi Leave = tryLeave
runLocationApi (Look locationId) = view locationId
runLocationApi (Chat text locationId) = chat text locationId
