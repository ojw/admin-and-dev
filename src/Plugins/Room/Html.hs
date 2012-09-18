{-# LANGUAGE OverloadedStrings, FlexibleContexts #-}

module Plugins.Room.Html where


import Prelude hiding               ( div )

import Control.Monad                ( forM_ )
import Control.Monad.Trans          ( MonadIO )
{-
import Data.Acid                    ( AcidState(..) )
import Data.Acid.Advanced           ( query', update' )
import Happstack.Server.RqData
import Happstack.Server             ( Response, ServerPart, ServerPartT, ok
                                    , toResponse, simpleHTTP, nullConf
                                    , seeOther, dir, notFound, seeOther
                                    , Method(..), methodM, FilterMonad
                                    , WebMonad, ServerMonad, Happstack
                                    , mapServerPartT )
import Web.Routes                   ( RouteT, showURL )
import Data.Text
-}

import Text.Blaze.Html5.Attributes  as A
import Text.Blaze.Html5             as H

import Util.HasAcidState
--import Plugins.Auth
--}

import Plugins.Room.Acid.Core --            ( RoomId(..), RoomState )

chatInput :: Html
chatInput =
    div ! class_ "chat_input" $
        do  input ! class_ "chat_input" ! type_ "text" ! value "type some stuff"
            input ! class_ "chat_send" ! type_ "button" ! value "Send"

chatBox :: Html
chatBox =
    div ! class_ "chat_box" $ 
        do  textarea "chat text here" ! class_ "chat_display"
            chatInput 

createRoomButton :: Html
createRoomButton =
    div ! class_ "create_room" $
        do  input ! class_ "create_room_button" ! type_ "button" ! value "Create Room"

roomList :: [RoomId] -> Html
roomList rooms =
    div ! class_ "room_list" $
        do  div ! class_ "room_list_header" 
                $ toHtml $ "Rooms: (" ++ show (length rooms) ++ ")"
            ul  $ forM_ rooms (li . toHtml . _unRoomId)

roomBox :: [RoomId] -> Html
roomBox rooms =
    div ! class_ "room_box" $
        do  roomList rooms
            createRoomButton

roomBox' :: (Functor m, MonadIO m, HasAcidState m RoomState) => m Html
roomBox' =
    do  rooms <- query LookRooms
        return $ roomBox $ fmap _roomId rooms 
