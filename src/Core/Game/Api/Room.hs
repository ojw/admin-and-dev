{-# LANGUAGE DeriveDataTypeable, FlexibleContexts, GeneralizedNewtypeDeriving
  , MultiParamTypeClasses, OverloadedStrings, ScopedTypeVariables
  , TemplateHaskell, TypeFamilies, FlexibleInstances, RecordWildCards
  , TypeOperators #-}

module Core.Game.Api.Location where

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
import Core.Game.Acid.Procedures -- .Location
import Core.Game.Acid.Types.Location
import Core.Game.Json.Location

processLocationRequest 
    ::  ( Happstack m, MonadIO m
        , Typeable p, Typeable s, Typeable o
        , SafeCopy p, SafeCopy s, SafeCopy o
        )
    =>  UserId -> LocationId -> AcidState (GameAcid p s o) -> ByteString -> m Response
processLocationRequest userId roomId gameAcid json =
        case decode json :: Maybe LocationAPIRequest of
            Nothing         -> ok $ toResponse $ ("Yeah that request body didn't have the right stuff." :: String)
            Just request    -> do t <- runLocationAPI userId roomId request gameAcid
                                  ok $ toResponse $ t
 
data LocationAPIRequest
    = RequestSetLocation UserId (Maybe Location)
    | RequestGetLocation UserId
    deriving (Ord, Eq, Data, Typeable, Read, Show)

instance FromJSON LocationAPIRequest where
    parseJSON (Object o) =
        do
            (rqType :: Text) <- o .: "type"
            case rqType of
                "send"      -> o .: "message" >>= \msg -> return $ RequestSend msg
                "receive"   -> return RequestReceive
                "look"      -> return RequestLook
    parseJSON _ = mzero

-- probably needs some error handling which will have to come from eventual Location.Acid
runLocationAPI 
    ::  ( MonadIO m, Happstack m
        , Typeable p, Typeable s, Typeable o
        , SafeCopy p, SafeCopy s, SafeCopy o
        ) 
    => UserId -> LocationId -> LocationAPIRequest -> AcidState (GameAcid p s o) -> m Response -- Text
runLocationAPI userId roomId request gameAcid =
        case request of
            RequestSend msg      -> do update' gameAcid (Send userId roomId msg)
                                       ok $ toResponse $ ("Success" :: Text)    -- so dumb
            RequestReceive       -> do chat <- query'  gameAcid (Receive userId roomId)
                                       ok $ toResponse $ encode chat            -- good for now
            RequestLook          -> do rooms <- query'  gameAcid (LookLocations)
                                       ok $ toResponse $ encode rooms
