{-# LANGUAGE MultiParamTypeClasses, DeriveDataTypeable, GeneralizedNewtypeDeriving, Rank2Types #-}

module Game where

import Data.Aeson       ( ToJSON, FromJSON )
import Data.Acid
import Data.Data
import Data.Text
import Prelude hiding   ( lookup )
import Data.Map
import Data.IxSet       ( Indexable )
import Happstack.Server -- ( Response )
import Data.SafeCopy    ( SafeCopy )

import Data.ByteString.Lazy.Char8 as L ( ByteString, pack )

import Util.HasAcidState
import Core.Auth.Acid   ( UserId )

newtype GameId = GameId { unGameId :: Int } deriving (Ord, Eq, Read, Show, Data, Typeable, SafeCopy)

-- will be more complex later to allow rankings in many-player games
data Outcome = Won | Loss | Draw

data GenericGameOutcome player = GenericGameOutcome [(player, Outcome)]
newtype GameOutcome = GameOutcome [(UserId, Outcome)]

-- need SafeCopy instance for state; might demand it here, but this is supposed to be pretty abstract
data TurnBasedGame player state outcome display command options = TurnBasedGame
    { runCommand    :: player -> command -> state -> Either outcome state
    , getView       :: player -> Either outcome state -> display -- this one might change -- not sure if display needs to cover outcomes
    , newGame       :: options -> state
    } 

-- as a web game serving html / js, mostly we're receiving and returning bytestrings
-- might actually switch to storing outcomes on disc? or entire games for replay
-- SafeCopy constraint is necessary, but not sure if this is right way to enforce it
data ThisKindaGame state = ThisKindaGame
    { thisRunCommand  :: (SafeCopy state) => UserId -> ByteString -> state -> Either GameOutcome state 
    , thisGetView     :: (SafeCopy state) => UserId -> Either GameOutcome state -> ByteString
    , thisNewGame     :: (SafeCopy state) => ByteString -> state
    }

-- for Html games on this server, this is what we need to store
-- the actual state might also look like [(command, state)] to allow for game replay later
-- or just [state], since logging in state will probably provide info on the commands
data GameState player state = GameState
    { gameId :: GameId
    , state :: state -- Either outcome state ??? Or does something special happen when the game ends?  (Probably write something to disc, stop storing in ram.
    , players :: Map UserId player -- possibly an IxSet of (UserId, player) instead since we need lookup in both directions
    }

-- all the pieces we need to translate from an IdealGame to a ThisKindaGame
data Html5Client outcome display command options player = Html5Client
    { encodeDisplay     :: display -> ByteString
    , decodeCommand     :: ByteString -> command
    , decodeOptions     :: ByteString -> options
    , convertOutcome    :: outcome -> GenericGameOutcome player -- then the server can convert player to UserId
    }

clientRunCommand
    :: UserId 
    -> ByteString 
    -> GameState player state 
    -> TurnBasedGame player state outcome display command options
    -> Html5Client outcome display command options player
    -> Either outcome state
clientRunCommand userId json acidState gameType client =
    case lookup userId (players acidState) of
        Nothing     -> Right (state acidState)
        Just player -> let  command     = (decodeCommand client) json
                            gameState   = state acidState in
                            (runCommand gameType) player command gameState

clientGetView
    :: UserId 
    -> ByteString 
    -> GameState player state 
    -> TurnBasedGame player state outcome display command options
    -> Html5Client outcome display command options player
    -> ByteString
clientGetView userId json acidState gameType client =
    case lookup userId (players acidState) of
        Nothing     -> L.pack "User not a player in game."
        Just player -> (encodeDisplay client) $ (getView gameType) player (Right (state acidState))

clientNewGame
    :: ByteString 
    -> TurnBasedGame player state outcome display command options
    -> Html5Client outcome display command options player
    -> state
clientNewGame json gameType client =
    (newGame gameType) $ (decodeOptions client) json
