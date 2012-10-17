{-# LANGUAGE DeriveDataTypeable, FlexibleContexts, GeneralizedNewtypeDeriving
  , MultiParamTypeClasses, OverloadedStrings, ScopedTypeVariables
  , TemplateHaskell, TypeFamilies, FlexibleInstances, RecordWildCards
  , TypeOperators #-}

module Core.Game.Api.Room where

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
import Core.Auth.Auth
import Util.HasAcidState
import Util.GetBody
import Core.Room.Acid
import Core.Room.Json as Json

processRoomRequest :: (HasAcidState m RoomState, Happstack m, MonadIO m)
                   => UserId -> RoomId -> ByteString -> m Response
processRoomRequest userId roomId json =
    do
        case decode json :: Maybe RoomAPIRequest of
            Nothing         -> ok $ toResponse $ ("Yeah that request body didn't have the right stuff." :: String)
            Just request    -> do t <- runRoomAPI userId roomId request -- will be uid roomId request
                                  ok $ toResponse $ t
 
{-
processRoomRequest :: (HasAcidState m RoomState, HasAcidState m AuthState, HasAcidState m ProfileState, Happstack m, MonadIO m)
                   => UserId -> ByteString -> m Response
processRoomRequest uid json =
    do
        case decode json :: Maybe RoomAPIRequest of
            Nothing         -> ok $ toResponse $ ("Yeah that request body didn't have the right stuff." :: String)
            Just request    -> do t <- runRoomAPI uid request
                                  ok $ toResponse $ t
-}
------------------------------------------------------------------

-- type to represent room API requests from json

-- actually, should be able to just write a FromJSON instance
-- for the Acidic types from Room.Acid.
-- however, the acidic types all want username as input
-- so either remove that requirement or keep as is

data RoomAPIRequest
    = RequestCreate Int
--    | RequestJoin RoomId
--    | RequestLeave
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
--                "join"      -> o .: "roomId" >>= \rid -> return $ RequestJoin (RoomId rid)
--                "leave"     -> return RequestLeave
                "send"      -> o .: "message" >>= \msg -> return $ RequestSend msg
                "receive"   -> return RequestReceive
                "look"      -> return RequestLook
    parseJSON _ = mzero

-- probably needs some error handling which will have to come from eventual Room.Acid
runRoomAPI :: (HasAcidState m RoomState, MonadIO m, Happstack m) 
           => UserId -> RoomId -> RoomAPIRequest -> m Response -- Text
runRoomAPI userId roomId request =
    do
        roomState :: AcidState RoomState <- getAcidState
        case request of
--            RequestCreate cap    -> do (RoomId rid) <- update' roomState (CreateRoom uid )
--                                       ok $ toResponse $ show rid               -- good for now
--            RequestJoin rid      -> do update' roomState (JoinRoom uid rid)
--                                       ok $ toResponse $ ("Success" :: Text)    -- this is the wrong behavior
--            RequestLeave         -> do update' roomState (LeaveRoom uid)
--                                       ok $ toResponse $ ("Success" :: Text)    -- wrong again
            RequestSend msg      -> do update' roomState (Send userId roomId msg)
                                       ok $ toResponse $ ("Success" :: Text)    -- so dumb
            RequestReceive       -> do chat <- query'  roomState (Receive userId roomId)
                                       ok $ toResponse $ encode chat            -- good for now
            RequestLook          -> do rooms <- query'  roomState (LookRooms)
                                       ok $ toResponse $ encode rooms -- Json.encode $ [(1 :: Int),(2 :: Int),(3 :: Int)]           -- good for now
