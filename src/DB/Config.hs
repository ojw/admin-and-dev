module DB.Config where

import Network

data DBConfig = DBConfig
    { authLocation :: (HostName, PortID)
    , profileLocation :: (HostName, PortID)
    , locationLocation :: (HostName, PortID)
    }

defaultDBConfig = DBConfig
    { authLocation      = ("localhost", UnixSocket "61234")
    , profileLocation   = ("localhost", UnixSocket "61235")
    , locationLocation  = ("localhost", UnixSocket "61236")
    }
