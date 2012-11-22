{-# LANGUAGE OverloadedStrings #-}

module RockPaperScissors where

import Control.Monad            ( mzero )
import Data.Aeson               ( FromJSON, parseJSON, (.:), decode )
import Data.Aeson.Types
import Data.Text                ( Text )
import Data.ByteString.Lazy

data RPSPlayer = Player1 | Player2 deriving (Ord, Eq, Read, Show)

data RPSOutcome = Win | Loss | Draw deriving (Ord, Eq, Read, Show)

data RPSStatus = Ongoing | Over deriving (Ord, Eq, Read, Show)

data RPSThrow = Rock | Paper | Scissors deriving (Ord, Eq, Read, Show)

data RPSGameOutcome = RPSGameOutcome
    { player1Outcome    :: RPSOutcome
    , player2Outcome    :: RPSOutcome
    } deriving (Ord, Eq, Read, Show)

data RPSGameState = RPSGameState
    { player1Throw  :: Maybe RPSThrow
    , player2Throw  :: Maybe RPSThrow
    } deriving (Ord, Eq, Read, Show)

addThrow :: RPSPlayer -> RPSThrow -> Either RPSGameOutcome RPSGameState -> Either RPSGameOutcome RPSGameState
addThrow _ _ (Left outcome) = Left outcome
addThrow player throw (Right state) =
    case player of
        Player1 ->  case player1Throw state of
                        Just t  -> Right state
                        Nothing -> Right $ state { player1Throw = Just throw }
        Player2 -> case player2Throw state of
                        Just t  -> Right state
                        Nothing -> Right $ state { player2Throw = Just throw }

checkOutcome :: Either RPSGameOutcome RPSGameState -> Either RPSGameOutcome RPSGameState 
checkOutcome (Left outcome) = Left outcome
checkOutcome (Right state)  = 
    case (player1Throw state, player2Throw state) of
        (Nothing, _)                    -> Right state
        (_, Nothing)                    -> Right state
        (Just Rock, Just Rock)          -> Left $ RPSGameOutcome Draw Draw
        (Just Rock, Just Paper)         -> Left $ RPSGameOutcome Loss Win
        (Just Rock, Just Scissors)      -> Left $ RPSGameOutcome Win Loss
        (Just Paper, Just Rock)         -> Left $ RPSGameOutcome Win Loss
        (Just Paper, Just Paper)        -> Left $ RPSGameOutcome Draw Draw
        (Just Paper, Just Scissors)     -> Left $ RPSGameOutcome Loss Win
        (Just Scissors, Just Rock)      -> Left $ RPSGameOutcome Loss Win   
        (Just Scissors, Just Paper)     -> Left $ RPSGameOutcome Win Loss
        (Just Scissors, Just Scissors)  -> Left $ RPSGameOutcome Draw Draw

processCommand :: RPSPlayer -> RPSThrow -> Either RPSGameOutcome RPSGameState -> Either RPSGameOutcome RPSGameState
processCommand player throw state = checkOutcome $ addThrow player throw state

processRawCommand :: RPSPlayer -> ByteString -> Either RPSGameOutcome RPSGameState -> Either RPSGameOutcome RPSGameState
processRawCommand player rawThrow state = 
    case decode rawThrow :: Maybe RPSThrow of
        Just throw  -> processCommand player throw state
        Nothing     -> state

newGame :: RPSGameState
newGame = RPSGameState Nothing Nothing

instance FromJSON RPSThrow where
    parseJSON (Object o) = do   throw <- o .: "Throw"
                                return $ (read throw :: RPSThrow)
    parseJSON _ = mzero
