{-# LANGUAGE OverloadedStrings #-}

module Client.Html.Matchmaker where

import Prelude hiding ( div )

import Text.Blaze.Html5 as H
import Text.Blaze.Html5.Attributes as A

import Client.Html.Room

leaveMatchmakerButton :: Html
leaveMatchmakerButton = button ! class_ "matchmaker leave" $ "Back to Lobby"

players :: Html
players = div ! class_ "matchmaker players" $
            p "Players:"
            div ! class_ "matchmaker players list" $ "players go here"

matchmaker :: Html
matchmaker = div ! class_ "matchmaker" $ do
            players
            chatBox
            leaveMatchmakerButton            
