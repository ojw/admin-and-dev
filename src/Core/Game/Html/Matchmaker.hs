{-# LANGUAGE OverloadedStrings #-}

module Core.Game.Html.Matchmaker where

import Prelude hiding ( div )

import Text.Blaze.Html5 as H
import Text.Blaze.Html5.Attributes as A

import Core.Game.Html.Room

leaveMatchmakerButton :: Html
leaveMatchmakerButton = button ! class_ "matchmaker leave" $ "Back to Lobby"

players :: Html
players = div ! class_ "matchmaker players" $
                div ! class_ "lobby label open" $ do
                    p "Players:"
                    div ! class_ "lobby players list" $ "[players will go here]"

matchmaker :: Html
matchmaker = div ! class_ "lobby" $ do
            players
            chatBox
            leaveMatchmakerButton            
