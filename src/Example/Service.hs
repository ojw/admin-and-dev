{-# LANGUAGE Rank2Types, OverloadedStrings, ScopedTypeVariables #-}

module Example.Service where

import Control.Monad.Trans
import Control.Monad
import Util.HasAcidState
import Data.ByteString.Lazy
import Happstack.Server
import Data.Aeson
import Data.Aeson.Types
import Data.Text
import Data.Maybe ( fromMaybe )
import Data.Acid hiding (query)
import Data.Acid.Advanced

import Util.GetBody
import Example.Acid
import Example.App
import Server.Game.Acid.Types.Room
import Server.Auth.Auth
import Server.Location.Acid
import Server.Game.Acid.Types.Lobby
import Server.Game.Handler
import Server.Game.Acid.GameAcid

routeService :: App Response
routeService =
    do  mUserId <- getUserId'
        body <- getBody
        liftIO $ Data.ByteString.Lazy.putStrLn body
        case mUserId of
            Nothing  -> ok $ toResponse ("Not logged in!" :: Text) -- should not be ok
            Just uid -> do  locationState :: AcidState (Server.Location.Acid.LocationState Games) <- getAcidState
                            game :: Maybe Games <- query' locationState $  Server.Location.Acid.GetLocation uid
                            case game of
                                Just Dummy  ->
                                    --do  gameAcid :: AcidState GameHolder <- getAcidState
                                    --    gameRouter uid gameAcid body
                                    ok $ toResponse $ ("Hey there, things are commented out right now.  You are in a dummy game or something." :: Text)
                                Nothing     -> do
                                    ok $ toResponse $ ("You are not in a game yet.  There will be a game selection screen here." :: Text)
