{-# LANGUAGE DeriveDataTypeable, FlexibleContexts, GeneralizedNewtypeDeriving
  , MultiParamTypeClasses, OverloadedStrings, ScopedTypeVariables
  , TemplateHaskell, TypeFamilies, FlexibleInstances, RecordWildCards
  , TypeOperators #-}

module Core.Game.Api.Room where

import Prelude hiding ( (.) )
import Control.Category ( (.) )
import Control.Monad.Trans
import Data.Data
import Data.SafeCopy
import Data.Acid hiding ( query )
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

import Util.HasAcidState
import Core.Profile.Acid
import Core.Auth.Acid
import Core.Game.Acid.GameAcid
import Core.Game.Acid.Procedures -- .Room
import Core.Game.Acid.Types.Room
import Core.Game.Json.Room

processRoomRequest 
    ::  ( Happstack m, MonadIO m
        , Typeable p, Typeable s, Typeable o
        , SafeCopy p, SafeCopy s, SafeCopy o
        , Ord s, Ord p, Ord o
        , HasAcidState m Core.Profile.Acid.ProfileState
        )
    =>  UserId -> AcidState (GameAcid p s o) -> ByteString -> m Response
processRoomRequest userId gameAcid json =
        case decode json :: Maybe RoomAPIRequest of
            Nothing         -> ok $ toResponse $ ("Yeah that request body didn't have the right stuff." :: String)
            Just request    -> do mRoomId <- query' gameAcid (GetRoomId userId)
                                  case mRoomId of 
                                    Just roomId ->runRoomAPI userId roomId request gameAcid
                                    Nothing     -> ok $ toResponse $ ("You... seem not to be in a room." :: String)

 
data RoomAPIRequest
    = RequestSend Text
    | RequestReceive
--    | RequestLook
    deriving (Ord, Eq, Data, Typeable, Read, Show)

instance FromJSON RoomAPIRequest where
    parseJSON (Object o) =
        do
            (rqType :: Text) <- o .: "type"
            case rqType of
                "send"      -> o .: "message" >>= \msg -> return $ RequestSend msg
                "receive"   -> return RequestReceive
--                "look"      -> return RequestLook
    parseJSON _ = mzero

runRoomAPI 
    ::  ( MonadIO m, Happstack m
        , Typeable p, Typeable s, Typeable o
        , SafeCopy p, SafeCopy s, SafeCopy o
        , HasAcidState m Core.Profile.Acid.ProfileState
        ) 
    => UserId -> RoomId -> RoomAPIRequest -> AcidState (GameAcid p s o) -> m Response -- Text
runRoomAPI userId roomId request gameAcid =
        case request of
            RequestSend msg      -> do userName <- query $ AskName userId
                                       update' gameAcid (Send (maybe (UserName "Unknown!") id userName) roomId msg) -- DANGER!
                                       ok $ toResponse $ ("Success" :: Text)    -- so dumb
            RequestReceive       -> do chat <- query'  gameAcid (Receive roomId)
                                       ok $ toResponse $ encode chat            -- good for now
--            RequestLook          -> do rooms <- query'  gameAcid (LookRooms)
--                                       ok $ toResponse $ encode rooms
