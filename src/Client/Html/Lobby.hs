{-# LANGUAGE OverloadedStrings #-}

module Client.Html.Lobby where

import Prelude hiding ( div )

import Text.Blaze.Html5 as H
import Text.Blaze.Html5.Attributes as A

import Client.Html.Room

newGameButton :: Html
newGameButton = button ! class_ "lobby create" $ "New Game"

openGames :: Html
openGames = div ! class_ "lobby open" $
                div ! class_ "lobby label open" $ do
                    p "Open Games:"
                    div ! class_ "lobby open list" $ "[open games will go here]"

lobby :: Html
lobby = div ! class_ "lobby" $ do
            chatBox
            openGames
            newGameButton            
