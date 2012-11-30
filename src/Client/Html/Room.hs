{-# LANGUAGE OverloadedStrings, FlexibleContexts #-}

module Client.Html.Room where


import Prelude hiding               ( div )

import Text.Blaze.Html5.Attributes  as A
import Text.Blaze.Html5             as H

chatTextInput :: Html
chatTextInput = input ! class_ "chat_input" ! type_ "text" ! value "type some stuff"

sendChatButton :: Html
sendChatButton = input ! class_ "chat_send" ! type_ "submit" ! value "Send"

chatInput :: Html
chatInput =
    div ! class_ "chat_input" $ H.form $
            do  chatTextInput
                sendChatButton

chatBox :: Html
chatBox =
    div ! class_ "chat_box" $ 
        do  textarea "chat text goes here" ! class_ "chat_display" ! readonly "true" ! cols "50" ! rows "20"
            chatInput 
