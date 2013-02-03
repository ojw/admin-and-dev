module Framework.Api where

import Framework.Location.Internal.Api ( runLocationApi, LocationApi )
import Framework.Location.Internal.Views.LocationView ( LocationView )
import Framework.Location.Internal.Types.Location ( MonadLocationAction )

data FrameworkApi = FWLocationApi LocationApi

data FrameworkView = FWLocationView LocationView

-- constraint should be MonadFrameworkAction
-- still need write a wrapper datatype holding all the state types
runApi :: (MonadLocationAction m) => FrameworkApi -> m FrameworkView
runApi (FWLocationApi locationApi) = runLocationApi locationApi >>= return . FWLocationView
