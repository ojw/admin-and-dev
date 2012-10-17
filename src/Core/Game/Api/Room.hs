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

import Core.Auth.Acid
import Core.Game.Acid.GameAcid
import Core.Game.Acid.Procedures -- .Room
import Core.Game.Acid.Types.Room
import Core.Game.Json.Room

processRoomRequest 
    ::  ( Happstack m, MonadIO m
        , Typeable p, Typeable s, Typeable o
        , SafeCopy p, SafeCopy s, SafeCopy o
        )
    => UserId -> RoomId -> AcidState (GameAcid p s o) -> ByteString -> m Response
processRoomRequest userId roomId gameAcid json =
        case decode json :: Maybe RoomAPIRequest of
            Nothing         -> ok $ toResponse $ ("Yeah that request body didn't have the right stuff." :: String)
            Just request    -> do t <- runRoomAPI userId roomId request gameAcid -- will be uid roomId request
                                  ok $ toResponse $ t
 
data RoomAPIRequest
    = RequestSend Text
    | RequestReceive
    | RequestLook
    deriving (Ord, Eq, Data, Typeable, Read, Show) -- all necessary?

instance FromJSON RoomAPIRequest where
    parseJSON (Object o) =
        do
            (rqType :: Text) <- o .: "type"
            case rqType of
                "send"      -> o .: "message" >>= \msg -> return $ RequestSend msg
                "receive"   -> return RequestReceive
                "look"      -> return RequestLook
    parseJSON _ = mzero

-- probably needs some error handling which will have to come from eventual Room.Acid
runRoomAPI 
    ::  ( MonadIO m, Happstack m
        , Typeable p, Typeable s, Typeable o
        , SafeCopy p, SafeCopy s, SafeCopy o
        ) 
    => UserId -> RoomId -> RoomAPIRequest -> AcidState (GameAcid p s o) -> m Response -- Text
runRoomAPI userId roomId request gameAcid =
        case request of
            RequestSend msg      -> do update' gameAcid (Send userId roomId msg)
                                       ok $ toResponse $ ("Success" :: Text)    -- so dumb
            RequestReceive       -> do chat <- query'  gameAcid (Receive userId roomId)
                                       ok $ toResponse $ encode chat            -- good for now
            RequestLook          -> do rooms <- query'  gameAcid (LookRooms)
                                       ok $ toResponse $ encode rooms