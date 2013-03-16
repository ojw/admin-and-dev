module DB.Server where

import Network
import Control.Concurrent

import Data.Maybe                   ( fromMaybe )
import Data.Acid                    ( AcidState, closeAcidState )
import Data.Acid.Local              ( createCheckpointAndClose 
                                    , openLocalStateFrom, openLocalState )
import Data.Acid.Remote
import Control.Lens
import Data.SafeCopy
import Data.Data
import Data.Typeable
import System.FilePath              ( (</>) )
import Control.Exception            ( bracket )
import Control.Lens

import Common.Location.Instances.Create
import Common.Auth.Types
import Common.Profile.Types

import DB.Acid

type DBLocation = (HostName, PortID, FilePath)

data DBConfig = DBConfig
    { authLocation :: DBLocation
    , profileLocation :: DBLocation
    , locationLocation :: DBLocation
    }

defaultDBConfig = DBConfig
    { authLocation      = ("localhost", UnixSocket "AuthDBSocket", "/AuthState")
    , profileLocation   = ("localhost", UnixSocket "ProfileDBSocket", "/ProfileState")
    , locationLocation  = ("localhost", UnixSocket "LocationDBSocket", "/LocationState")
    }

withAcid :: Maybe FilePath -- ^ state directory
         -> (Acid -> IO a) -- ^ action
         -> IO a
withAcid mBasePath f =
    let basePath = fromMaybe ".state" mBasePath in
    bracket (openLocalStateFrom (basePath </> "auth")       initialAuthState)       (createCheckpointAndClose) $ \auth ->
    bracket (openLocalStateFrom (basePath </> "profile")    initialProfileState)    (createCheckpointAndClose) $ \profile ->
    bracket (openLocalStateFrom (basePath </> "location")   initialLocationState)   (createCheckpointAndClose) $ \location ->
        f (Acid auth profile location )

withRemoteAcid :: Maybe DBConfig
               -> (Acid -> IO a)
               -> IO a
withRemoteAcid mDBConfig f =
    let dbConfig = fromMaybe defaultDBConfig mDBConfig in
    bracket (openRemoteState (view _1 $ authLocation dbConfig) (view _2 $ authLocation dbConfig))       closeAcidState $ \auth ->
    bracket (openRemoteState (view _1 $ profileLocation dbConfig) (view _2 $ authLocation dbConfig))    closeAcidState $ \profile ->
    bracket (openRemoteState (view _1 $ locationLocation dbConfig) (view _2 $ authLocation dbConfig))   closeAcidState $ \location ->
        f (Acid auth profile location)

startDBServers :: Maybe DBConfig -> IO ()
startDBServers mDBConfig = do
    let dbConfig = fromMaybe defaultDBConfig mDBConfig
    forkIO $ bracket (openLocalState initialAuthState) closeAcidState (\auth -> acidServer auth (view _2 $ authLocation dbConfig))
    forkIO $ bracket (openLocalState initialProfileState) closeAcidState (\profile -> acidServer profile (view _2 $ profileLocation dbConfig))
    forkIO $ bracket (openLocalState initialLocationState) closeAcidState (\location -> acidServer location (view _2 $ locationLocation dbConfig))
    return ()
