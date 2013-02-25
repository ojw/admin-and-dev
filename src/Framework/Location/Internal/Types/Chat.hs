
module Framework.Location.Internal.Types.Chat where

import Data.Text                    ( Text )
import Framework.Profile    ( UserName )

type Chat = (UserName, Text)

type ChatHolder = [Chat]

addChat :: Chat -> ChatHolder -> ChatHolder
addChat = (:)
