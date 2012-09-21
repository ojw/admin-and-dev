{-# LANGUAGE DeriveDataTypeable, FlexibleContexts, GeneralizedNewtypeDeriving
  , MultiParamTypeClasses, OverloadedStrings, ScopedTypeVariables
  , TemplateHaskell, TypeFamilies, FlexibleInstances, RecordWildCards
  , TypeOperators #-}

module Plugins.Room.Api where

import Prelude hiding ( (.) )
import Control.Category ( (.) )
import Control.Monad.Trans
import Data.Data
import Data.Acid
import Data.Acid.Advanced
import Data.Aeson
import Data.Text as Text
import Data.Text.Encoding   ( decodeUtf8 )

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
import Plugins.Room.Acid.Core
import Plugins.Room.Acid.Json as Json

-- URL routing for the Room API

data RoomAPIURL
    = RoomAPIDefault -- probably won't actually use routing beyond this
    | RoomAPICreate
    | RoomAPIJoin
    | RoomAPILeave
    | RoomAPISend
    | RoomAPIReceive
    | RoomAPILook

$(derivePrinterParsers ''RoomAPIURL)

roomAPIBoomerang :: Router () (RoomAPIURL :- ())
roomAPIBoomerang =
    (  rRoomAPIDefault
    <> rRoomAPICreate . (lit "create")
    <> rRoomAPIJoin . (lit "join")
    <> rRoomAPILeave . (lit "leave")
    <> rRoomAPISend . (lit "send")
    <> rRoomAPIReceive . (lit "receive")
    <> rRoomAPILook . (lit "look")
    )

-- maps a URL in the Room API to a response
-- need function to get json Value from request body
processRoomURL :: (HasAcidState m RoomState, HasAcidState m AuthState, HasAcidState m ProfileState, Happstack m) 
               => RoomAPIURL -> RouteT RoomAPIURL m Response
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
     => RoomAPIURL -> RouteT RoomAPIURL m Response
temp url = ok $ toResponse ("FOOOOOOOOOOOOOO" :: String)

roomAPISite :: (Happstack m, HasAcidState m RoomState, HasAcidState m AuthState, HasAcidState m ProfileState) 
            => Site RoomAPIURL (m Response)
roomAPISite = boomerangSite (runRouteT processRoomURL) roomAPIBoomerang

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
runRoomAPI :: (HasAcidState m RoomState, MonadIO m, Happstack m) 
           => UserId -> RoomAPIRequest -> m Response -- Text
runRoomAPI uid request =
    do
        roomState :: AcidState RoomState <- getAcidState
        case request of
            RequestCreate cap    -> do (RoomId rid) <- update' roomState (CreateRoom uid )
                                       ok $ toResponse $ show rid               -- good for now
            RequestJoin rid      -> do update' roomState (JoinRoom uid rid)
                                       ok $ toResponse $ ("Success" :: Text)    -- this is the wrong behavior
            RequestLeave         -> do update' roomState (LeaveRoom uid)
                                       ok $ toResponse $ ("Success" :: Text)    -- wrong again
            RequestSend msg      -> do update' roomState (Send uid msg)
                                       ok $ toResponse $ ("Success" :: Text)    -- so dumb
            RequestReceive       -> do chat <- query'  roomState (Receive uid)
                                       ok $ toResponse $ encode chat            -- good for now
            RequestLook          -> do rooms <- query'  roomState (LookRooms)
                                       ok $ toResponse $ encode rooms -- Json.encode $ [(1 :: Int),(2 :: Int),(3 :: Int)]           -- good for now
