module Common.Location.View where

import Data.Text

import Common.Profile.Types
import Common.Location.Types

data GameView = GameView
    { gvGameId        :: GameId
    , gvLobbyId       :: LobbyId
    , gvChats         :: ChatHolder
    , gvMembers       :: [UserName]
    } deriving (Ord, Eq, Read, Show)

data LobbyView = LobbyView
    { lvName          :: Text
    , lvDescription   :: Text
    , lvLobbyId       :: LobbyId
    , lvChats         :: ChatHolder
    , lvMembers       :: [UserName]
    , lvMatchmakers   :: [MatchmakerGlimpse]
    } deriving (Ord, Eq, Read, Show) 

data LobbyGlimpse = LobbyGlimpse
    { lgName              :: Text
    , lgDescription       :: Text
    , lgLobbyId           :: LobbyId
    , lgMemberCount       :: Int
    , lgMatchmakerCount   :: Int
    } deriving (Ord, Eq, Read, Show) 

data MatchmakerView = MatchmakerView
    { mvMatchmakerId  :: MatchmakerId
    , mvChats         :: ChatHolder
    , mvCapacity      :: (Int, Int) -- (min, max)
    , mvOwner         :: UserName
    , mvMembers       :: [UserName]
    , mvLobbyId       :: LobbyId
    } deriving (Ord, Eq, Read, Show)

data MatchmakerGlimpse = MatchmakerGlimpse
    { mgMatchmakerId  :: MatchmakerId
    , mgCapacity      :: (Int, Int) -- (min, max)
    , mgOwner         :: UserName
    , mgMembers       :: Int
    } deriving (Ord, Eq, Read, Show)

data LocationView
    = LVLobbyView LobbyView
    | LVMatchmakerView MatchmakerView
    | LVGameView GameView
    | LVMessage Text 
