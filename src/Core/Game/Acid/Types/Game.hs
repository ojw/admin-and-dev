{-# LANGUAGE MultiParamTypeClasses, DeriveDataTypeable, GeneralizedNewtypeDeriving, Rank2Types, StandaloneDeriving,
    FlexibleContexts, UndecidableInstances #-}

module Core.Game.Acid.Types.Game where

import Data.Aeson       ( ToJSON, FromJSON )
import Data.Functor
import Data.Acid
import Data.Maybe       ( fromJust )
import Data.Data
import Data.Text hiding ( map )
import Data.Lens
import Data.IxSet
import Happstack.Server -- ( Response )
import Data.SafeCopy    ( SafeCopy )

import Data.ByteString.Lazy.Char8 as L ( ByteString, pack )

import Util.HasAcidState
import Core.Auth.Acid               ( UserId )
import Core.Game.Acid.Types.Room    ( RoomId )
import Core.Game.Acid.Types.Lobby   ( LobbyId )
import Core.Game.Acid.Types.Options ( Options )

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
    { runCommand    :: player -> command -> (options, state) -> Either outcome state
    , getView       :: player -> Either outcome state -> display -- this one might change -- not sure if display needs to cover outcomes
    , newGame       :: options -> state
    } 
{-
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
    -> Game player state outcome
    -> TurnBasedGame player state outcome display command options
    -> Html5Client outcome display command options player
    -> Either outcome state
clientRunCommand userId json acidState gameType client =
    case lookup userId (_players acidState) of
        Nothing     -> Right (_state acidState)
        Just player -> case (decodeCommand client) json of
                        Nothing      -> Right (_state acidState)
                        Just command -> (runCommand gameType) player command (_state acidState)

clientGetView
    :: UserId 
    -> ByteString 
    -> Game player state outcome
    -> TurnBasedGame player state outcome display command options
    -> Html5Client outcome display command options player
    -> ByteString
clientGetView userId json acidState gameType client =
    case lookup userId (_players acidState) of
        Nothing     -> L.pack "User not a player in game."
        Just player -> (encodeDisplay client) $ (getView gameType) player (Right (_state acidState))

clientNewGame
    :: ByteString 
    -> TurnBasedGame player state outcome display command options
    -> Html5Client outcome display command options player
    -> Maybe state
clientNewGame json gameType client =
    (newGame gameType) <$> (decodeOptions client) json
-}
data Game player state outcome = Game
    { _gameId   :: GameId
    , _options  :: Options
    , _state    :: Either state outcome
    , _players  :: [(UserId,player)]
    , _roomId   :: RoomId
    , _lobbyId  :: LobbyId
    }

deriving instance (Ord player, Ord state, Ord outcome) => Ord (Game player state outcome)
deriving instance (Eq player, Eq state, Eq outcome) => Eq (Game player state outcome)
deriving instance (Read player, Read state, Read outcome) => Read (Game player state outcome)
deriving instance (Show player, Show state, Show outcome) => Show (Game player state outcome)
deriving instance (Data player, Data state, Data outcome) => Data (Game player state outcome)
deriving instance Typeable3 Game

instance Indexable (Game player state outcome) where
    empty = ixSet [ ixFun $ \game -> [ _gameId game ]
                  , ixFun $ \game -> [ _lobbyId game ]
                  , ixFun $ \game -> [ _roomId game ]
                  , ixFun $ \game -> map fst (_players game)
                  ]

data GameState player state outcome = GameState
    { _nextGameId   :: GameId
    , _games        :: IxSet (Game player state outcome)
    }

deriving instance (Ord player, Ord state, Ord outcome, Typeable player, Typeable state, Typeable outcome) => Ord (GameState player state outcome)
deriving instance (Eq player, Eq state, Eq outcome, Ord player, Ord state, Ord outcome, Typeable player, Typeable state, Typeable outcome) => Eq (GameState player state outcome)
deriving instance (Read player, Read state, Read outcome, Ord player, Ord state, Ord outcome, Typeable player, Typeable state, Typeable outcome) => Read (GameState player state outcome)
deriving instance (Show player, Show state, Show outcome, Ord player, Ord state, Ord outcome, Typeable player, Typeable state, Typeable outcome) => Show (GameState player state outcome)
deriving instance (Data player, Data state, Data outcome, Ord player, Ord state, Ord outcome) => Data (GameState player state outcome)
deriving instance Typeable3 GameState
