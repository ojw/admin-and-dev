{-# LANGUAGE MultiParamTypeClasses #-}

module Game where

import Data.Aeson       ( ToJSON, FromJSON )
import Data.Acid
import Data.Text
import Happstack.Server ( Response )
import Data.SafeCopy    ( SafeCopy )

import Data.ByteString.Lazy.Char8   ( ByteString )

import Util.HasAcidState

-- This is the only structure we really NEED for a Game.
-- Some other structure might be nice - should we insist on JSON?
-- Probably not, but we will probably always use it.
-- Text vs Lazy Bytestring?  Probably use Lazy Bytestring.
-- We take it from POST requests, don't decode it, pass it straight to the game.
-- We expect the game to use Aeson to decode JSON data from bytestring,
-- but game is welcome to do whatever it pleases.

class Game st where
    runCommand  :: (HasAcidState m st) => ByteString -> m Response
    getState    :: (HasAcidState m st) => m Response 
    newGame     :: (HasAcidState m st) => ByteString -> m () -- bytestring represents options

-- This is the essential behavior of a game.
-- Other structures will be added - possibly as a datatype - with areas for profile-type data,
-- maybe front-end goodies, etc.  
