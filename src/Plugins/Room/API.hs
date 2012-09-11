{-# LANGUAGE DeriveDataTypeable, FlexibleContexts, GeneralizedNewtypeDeriving
  , MultiParamTypeClasses, OverloadedStrings, ScopedTypeVariables
  , TemplateHaskell, TypeFamilies, FlexibleInstances, RecordWildCards
  , TypeOperators #-}

module API.RoomAPI where

import Control.Monad.Trans
import Data.Data
import Data.Acid
import Data.Acid.Advanced
import Data.Aeson
import Data.Text as Text

import Auth
import HasAcidState
import Room

data RoomAPI
    = RoomAPI_Create Int
    | RoomAPI_Join RoomId
    | RoomAPI_Leave
    | RoomAPI_Send Text
    | RoomAPI_Receive
    deriving (Ord, Eq, Data, Typeable, Read, Show) -- all necessary?

instance FromJSON RoomAPI where
    parseJSON (Object o) =
        do
            (rqType :: Text) <- o .: "type"
            case rqType of
                "create"    -> o .: "capacity" >>= \cap -> return $ RoomAPI_Create (read cap :: Int)
                "join"      -> o .: "roomId" >>= \rid -> return $ RoomAPI_Join (RoomId rid)
                "leave"     -> return RoomAPI_Leave
                "send"      -> o .: "message" >>= \msg -> return $ RoomAPI_Send msg
                "receive"   -> return RoomAPI_Receive

processRoomAPI :: (HasAcidState m RoomState, MonadIO m) => UserId -> RoomAPI -> m Text
processRoomAPI uid request =
    do
        (roomState :: AcidState RoomState) <- getAcidState
        case request of
            RoomAPI_Create cap    -> do (RoomId rid) <- update' roomState (CreateRoom uid cap)
                                        return $ Text.pack $ show rid
            RoomAPI_Join rid      -> do update' roomState (JoinRoom uid rid)
                                        return "Success" -- this is the wrong behavior
            RoomAPI_Leave         -> do update' roomState (LeaveRoom uid)
                                        return "Success" -- wrong again
            RoomAPI_Send msg      -> do update' roomState (Send uid msg)
                                        return "Success" -- so dumb
            RoomAPI_Receive       -> do chat <- query'  roomState (Receive uid)
                                        return "bla" -- not even trying
