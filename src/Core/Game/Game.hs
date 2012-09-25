{-# LANGUAGE MultiParamTypeClasses #-}

module Game where

import Data.Aeson       ( ToJSON, FromJSON )
import Data.Acid
import Data.Text
import Data.IxSet       ( Indexable )
import Happstack.Server ( Response )
import Data.SafeCopy    ( SafeCopy )

import Data.ByteString.Lazy.Char8 as L ( ByteString, pack )

import Util.HasAcidState
import Core.Auth.Acid   ( UserId )

-- This is all that we ask of a game.
-- We might try to require ToJson of st, but we can't know which parts of st to show to which player
-- so this must be defined by class.
-- Maaaaybe add encode :: st -> ByteString? But this is really just getState

class Indexable st => Game st where
    runCommand  :: UserId -> ByteString -> st -> st -- ByteString is JSON encoded command
    encodeState :: UserId -> st -> ByteString -- ByteString is JSON encoded parts of game state that user is allowed to know
    newGame     :: ByteString -> st -- use as (newGame bs :: WhateverGame); ByteString is JSON encoded game options

-- Games must provide their own javascript bits - how to display and interact with the state, etc.
-- (Or provide other front ends.)
-- I'm not sure how to make this extremely general - probably add typeclasses representing each reasonable client type we expect to support
-- This seems like a good idea presently.

-- Typeclass for each client type - initially probably just html + js.
-- Not much we can enforce about html or js.
-- I would expect the Html / Js to look the same for any st and just handle different states on the client side,
-- but this allows the game author to decide this (and makes the code legal)

class Game st => HasHtmlJsClient st where
    getClientHtml   :: st -> ByteString
    getClientJs     :: st -> ByteString

instance Game Int where
    runCommand _ _ i = succ i
    encodeState _ i = L.pack "FOO"
    newGame _ = 0
