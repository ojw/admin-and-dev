{-# LANGUAGE MultiParamTypeClasses, DeriveDataTypeable, GeneralizedNewtypeDeriving, Rank2Types, StandaloneDeriving #-}

module Core.Game.Acid.Types.Game where

import Data.Aeson       ( ToJSON, FromJSON )
import Data.Functor
import Data.Acid
import Data.Maybe       ( fromJust )
import Data.Data
import Data.Text hiding ( map )
-- import Prelude hiding   ( lookup )
-- import Data.Map
import Data.IxSet       ( Indexable )
import Happstack.Server -- ( Response )
import Data.SafeCopy    ( SafeCopy )

import Data.ByteString.Lazy.Char8 as L ( ByteString, pack )

import Util.HasAcidState
import Core.Auth.Acid   ( UserId )

newtype GameId = GameId { unGameId :: Int } deriving (Ord, Eq, Read, Show, Data, Typeable, SafeCopy)

-- will be more complex later to allow rankings in many-player games
data Outcome = Won | Loss | Draw

newtype GenericGameOutcome player = GenericGameOutcome [(player, Outcome)]
newtype GameOutcome = GameOutcome [(UserId, Outcome)]

genericToGameOutcome :: Eq player => GenericGameOutcome player -> [(UserId, player)] -> GameOutcome
genericToGameOutcome (GenericGameOutcome generic) userMap = GameOutcome $ map (\(u,p)->(u,fromJust (lookup p generic))) userMap

gameToGenericOutcome :: GameOutcome -> [(UserId, player)] -> GenericGameOutcome player
gameToGenericOutcome (GameOutcome game) userMap = GenericGameOutcome $ map (\(p,u)->(p,fromJust (lookup u game))) (reverseMap userMap)

reverseMap :: [(a,b)] -> [(b,a)]
reverseMap [] = []
reverseMap ((first, second):xs) = (second, first) : (reverseMap xs)

reverseLookup :: (Eq b) => [(a,b)] -> b -> Maybe a
reverseLookup m key = lookup key $ reverseMap m

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
    , thisNewGame     :: (SafeCopy state) => ByteString -> Maybe state -- because could be invalid config
    }

-- all the pieces we need to translate from an IdealGame to a ThisKindaGame
data Html5Client outcome display command options player = Html5Client
    { encodeDisplay     :: display -> ByteString
    , decodeCommand     :: ByteString -> Maybe command
    , decodeOptions     :: ByteString -> Maybe options
    , encodeOutcome     :: outcome -> GenericGameOutcome player -- then the server can convert player to UserId
    , decodeOutcome     :: GenericGameOutcome player -> outcome  -- then the server can convert player to UserId
    }

convertRunCommand
    :: Eq player
    => (player -> command -> state -> Either outcome state) 
    -> (ByteString -> Maybe command)
    -> (outcome -> GenericGameOutcome player)
    -> [(UserId,player)]
    -> (UserId -> ByteString -> state -> Either GameOutcome state)
convertRunCommand abstractRun commandDecoder outcomeDecoder playerMap = \ userId json state ->
    case lookup userId playerMap of
        Nothing     -> Right state
        Just player -> case commandDecoder json of
                        Nothing       -> Right state
                        Just command  -> case abstractRun player command state of
                                            Right state -> Right state
                                            Left outcome -> Left $ genericToGameOutcome (outcomeDecoder outcome) playerMap

convertGetView
    :: (player -> Either outcome state -> display)
    -> (display -> ByteString)
    -> (GenericGameOutcome player -> outcome )
    -> [(UserId,player)]
    -> (UserId -> Either GameOutcome state -> ByteString)
convertGetView getView convertDisplay convertOutcomes playerMap = \ userId outcomeOrState ->
    case lookup userId playerMap of
        Nothing     -> L.pack "User not a player in game."
        Just player -> convertDisplay $ getView player $ case outcomeOrState of
                                                            Left outcome -> Left $ convertOutcomes $ gameToGenericOutcome outcome playerMap
                                                            Right s      -> Right s

convertNewGame
    :: (options -> state)
    -> (ByteString -> Maybe options)
    -> (ByteString -> Maybe state)
convertNewGame toGame decodeOptions = \byteString ->
    toGame <$> decodeOptions byteString

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
        Just player -> case (decodeCommand client) json of
                        Nothing      -> Right (state acidState)
                        Just command -> (runCommand gameType) player command (state acidState)

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
    -> Maybe state
clientNewGame json gameType client =
    (newGame gameType) <$> (decodeOptions client) json

data GameState player state {-outcome-} = GameState
    { gameId :: GameId
    , state :: state --Either state outcome
    , players :: [(UserId,player)]
    }
{-
deriving instance (Ord player, Ord state, Ord outcome) => Ord (GameState player state outcome)
deriving instance (Eq player, Eq state, Eq outcome) => Eq (GameState player state outcome)
deriving instance (Read player, Read state, Read outcome) => Read (GameState player state outcome)
deriving instance (Show player, Show state, Show outcome) => Show (GameState player state outcome)
deriving instance (Data player, Data state, Data outcome) => Data (GameState player state outcome)
deriving instance Typeable3 GameState
-}
deriving instance (Ord player, Ord state) => Ord (GameState player state)
deriving instance (Eq player, Eq state) => Eq (GameState player state)
deriving instance (Read player, Read state) => Read (GameState player state)
deriving instance (Show player, Show state) => Show (GameState player state)
deriving instance (Data player, Data state) => Data (GameState player state)
deriving instance Typeable2 GameState
