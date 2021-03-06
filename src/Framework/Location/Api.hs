module Framework.Location.Api where

import Data.Text
import Control.Monad.Reader hiding ( join )
import Control.Monad.Error ( throwError )
import Control.Monad.State ( get )

import Framework.Profile as Profile
import Common.Location.Types
import DB.Location.LocationAction
import Common.Location.Instances.Create
import Framework.Location.Instances.Location
import Framework.Location.Classes hiding ( chat )
import Framework.Location.Instances.View.LocationView
import Common.Classes as Classes ( view, create, delete, delete', add' )
import qualified Framework.Location.Classes as Location
import Data.Text ( Text, pack )
import Common.Location.View

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
    userId <- getCurrentUserId
    oldLocationId <- getUserLocation userId
    setLocation locationId userId
    onLeave oldLocationId
    onJoin locationId 
    view locationId

tryJoin :: LocationId -> LocationAction LocationView
tryJoin locationId = do
    userId <- getCurrentUserId
    oldLocationId <- getUserLocation userId
    canLeave <- canLeave oldLocationId
    canJoin <- canJoin locationId
    if canJoin && canLeave then join locationId else view oldLocationId

tryLeave :: LocationAction LocationView
tryLeave = do
    userId <- getCurrentUserId
    currentLocationId <- getUserLocation userId
    exit <- exit currentLocationId
    tryJoin exit

leave :: LocationAction LocationView
leave = getCurrentUserId >>= getUserLocation >>= join

chat :: Text -> LocationId -> LocationAction LocationView
chat text locationId = do
    userName <- getCurrentUserName
    Location.chat (userName, text) locationId
    view locationId

create :: LocationOptions -> LocationAction LocationView
create locationOptions = add (Classes.create locationOptions) >>= view

delete :: LocationId -> LocationAction LocationView
delete locationId = do
    DB.Location.LocationAction.delete locationId
    return $ LVMessage $ pack "Deleted."

runLocationApi :: LocationApi -> LocationAction LocationView
runLocationApi (Join locationId) = tryJoin locationId
runLocationApi Leave = tryLeave
runLocationApi (Look locationId) = view locationId
runLocationApi (Chat text locationId) = chat text locationId
runLocationApi (Delete locationId) = Framework.Location.Api.delete locationId
runLocationApi (Create locationOptions) = Framework.Location.Api.create locationOptions
