{-# LANGUAGE DeriveDataTypeable, FlexibleContexts, GeneralizedNewtypeDeriving
  , MultiParamTypeClasses, OverloadedStrings, ScopedTypeVariables
  , TemplateHaskell, TypeFamilies, FlexibleInstances, RecordWildCards
  , TypeOperators #-}

module Plugins.Room.API where

import Prelude hiding ( (.) )
import Control.Category ( (.) )
import Control.Monad.Trans
import Data.Data
import Data.Acid
import Data.Acid.Advanced
import Data.Aeson
import Data.Text as Text

import Happstack.Server -- ( Happstack, Response )
import Web.Routes.Happstack

import Text.Boomerang.TH
import Web.Routes -- ( Site, runRouteT )
import Web.Routes.Boomerang

import Control.Monad ( mzero )
import Data.ByteString.Lazy as L

import Happstack.Auth
import Plugins.Auth
import Util.HasAcidState
import Util.GetBody
import Plugins.Room.Acid

-- URL routing for the Room API

data RoomAPI_URL
    = RoomAPI_Create
    | RoomAPI_Join
    | RoomAPI_Leave
    | RoomAPI_Send
    | RoomAPI_Receive
    | RoomAPI_Look

$(derivePrinterParsers ''RoomAPI_URL)

roomAPIBoomerang :: Router () (RoomAPI_URL :- ())
roomAPIBoomerang =
    (  rRoomAPI_Create . (lit "create")
    <> rRoomAPI_Join . (lit "join")
    <> rRoomAPI_Leave . (lit "leave")
    <> rRoomAPI_Send . (lit "send")
    <> rRoomAPI_Receive . (lit "receive")
    <> rRoomAPI_Look . (lit "look")
    )

-- maps a URL in the Room API to a response
-- need function to get json Value from request body
processRoomURL :: (HasAcidState m RoomState, HasAcidState m AuthState, HasAcidState m ProfileState, Happstack m) 
               => RoomAPI_URL -> RouteT RoomAPI_URL m Response
processRoomURL url =
    do
        body <- lift $ getBody
        mUserId <- lift $ getUserId'
        case mUserId of
            Nothing     -> ok $ toResponse $ ("You aren't logged in!" :: String) -- improve, obv
            Just uid    -> do   

                                case decode body :: Maybe RoomAPIRequest of
                                    Nothing         -> ok $ toResponse $ ("Yeah that request body didn't have the right stuff." :: String)
                                    Just request    -> do t <- lift $ runRoomAPI uid request
                                                          ok $ toResponse $ t

temp :: (HasAcidState m RoomState, HasAcidState m AuthState, HasAcidState m ProfileState, Happstack m) 
     => RoomAPI_URL -> RouteT RoomAPI_URL m Response
temp url = ok $ toResponse ("FOOOOOOOOOOOOOO" :: String)

roomAPISite :: (Happstack m, HasAcidState m RoomState, HasAcidState m AuthState, HasAcidState m ProfileState) 
            => Site RoomAPI_URL (m Response)
roomAPISite = boomerangSite (runRouteT processRoomURL) roomAPIBoomerang --processRoomURL) roomAPIBoomerang

------------------------------------------------------------------

-- type to represent room API requests from json

-- actually, should be able to just write a FromJSON instance
-- for the Acidic types from Room.Acid.
-- however, the acidic types all want username as input
-- so either remove that requirement or keep as is

data RoomAPIRequest
    = RequestCreate Int
    | RequestJoin RoomId
    | RequestLeave
    | RequestSend Text
    | RequestReceive
    | RequestLook
    deriving (Ord, Eq, Data, Typeable, Read, Show) -- all necessary?

instance FromJSON RoomAPIRequest where
    parseJSON (Object o) =
        do
            (rqType :: Text) <- o .: "type"
            case rqType of
                "create"    -> o .: "capacity" >>= \cap -> return $ RequestCreate (read cap :: Int)
                "join"      -> o .: "roomId" >>= \rid -> return $ RequestJoin (RoomId rid)
                "leave"     -> return RequestLeave
                "send"      -> o .: "message" >>= \msg -> return $ RequestSend msg
                "receive"   -> return RequestReceive
                "look"      -> return RequestLook
    parseJSON _ = mzero

-- probably needs some error handling which will have to come from eventual Room.Acid
runRoomAPI :: (HasAcidState m RoomState, MonadIO m) 
           => UserId -> RoomAPIRequest -> m Text
runRoomAPI uid request =
    do
        roomState :: AcidState RoomState <- getAcidState
        case request of
            RequestCreate cap    -> do (RoomId rid) <- update' roomState (CreateRoom uid cap)
                                       return $ Text.pack $ show rid
            RequestJoin rid      -> do update' roomState (JoinRoom uid rid)
                                       return "Success" -- this is the wrong behavior
            RequestLeave         -> do update' roomState (LeaveRoom uid)
                                       return "Success" -- wrong again
            RequestSend msg      -> do update' roomState (Send uid msg)
                                       return "Success" -- so dumb
            RequestReceive       -> do chat <- query'  roomState (Receive uid)
                                       return "bla" -- not even trying 
            RequestLook          -> do rooms <- query'  roomState (LookRooms)
                                       return $ Text.pack $ Prelude.concat $ Prelude.map (show . _unRoomId) rooms
