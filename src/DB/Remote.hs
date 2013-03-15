module DB.Remote where

import Data.Maybe
import Control.Concurrent
import Data.Acid
import Data.Acid.Remote

import DB.Acid
import DB.Config
import DB.SimpleDB

-- I'm not not in over my head writing the database server
startDBServers :: Maybe DBConfig -> Maybe Acid -> IO ()
startDBServers mDBConfig mAcid = do
    case mAcid of
        Nothing -> withAcid Nothing action
        Just acid -> action acid 
    where action = \acid -> do
            let dbConfig = fromMaybe defaultDBConfig mDBConfig
            forkIO $ acidServer (authState acid) (snd $ authLocation dbConfig)
            forkIO $ acidServer (profileState acid) (snd $ profileLocation dbConfig)
            forkIO $ acidServer (locationState acid) (snd $ locationLocation dbConfig)
            return ()
