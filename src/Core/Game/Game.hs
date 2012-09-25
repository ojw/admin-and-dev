{-# LANGUAGE MultiParamTypeClasses, DeriveDataTypeable, GeneralizedNewtypeDeriving #-}

module Game where

import Data.Aeson       ( ToJSON, FromJSON )
import Data.Acid
import Data.Data
import Data.Text
import Data.Map
import Data.IxSet       ( Indexable )
import Happstack.Server ( Response )
import Data.SafeCopy    ( SafeCopy )

import Data.ByteString.Lazy.Char8 as L ( ByteString, pack )

import Util.HasAcidState
import Core.Auth.Acid   ( UserId )

newtype GameId = GameId { unGameId :: Int } deriving (Ord, Eq, Read, Show, Data, Typeable, SafeCopy)


class IdealGame player state display command options where
    idealRunCommand  :: player -> command -> state -> state
    idealGetView     :: player -> state -> display
    idealNewGame     :: options -> state

class ThisKindaGame state where
    thisRunCommand  :: (HasAcidState m state) => UserId -> ByteString -> m ()
    thisGetView     :: (HasAcidState m state) => UserId -> m ByteString
    thisNewGame     :: (HasAcidState m state) => ByteString -> m ()

-- we have a translator object 

data Game player state display command options = Game
    { runCommand    :: player -> command -> state -> state
    , getView       :: player -> state -> display
    , newGame       :: options -> state
    } 

{-
data Html5Game state = Html5Game
    { runCommand    :: UserId -> ByteString -> state -> state
    , getView       :: UserId -> state -> ByteString
    , newGame       :: ByteString -> state
    }
-}

-- for Html games on this server, this is what we need to store
data GameState player state = GameState
    { gameId :: GameId
    , state :: state
    , players :: Map UserId player
    }

data Html5Client display command options player = Html5Client
    { encodeDisplay :: display -> ByteString
    , decodeCommand :: ByteString -> command
    , decodeOptions :: ByteString -> options
    }

{-
convertGameToHtml5 :: Game state player display command options -> Html5Client player display command options -> Html5Game state
convertGameToHtml5 (Game run get new) (Html5Client encDis, decCom, decOpt) = 
    let newRunCommand = \userId byteString state -> runCommand (getPlayerWithId userId) 
-} 
