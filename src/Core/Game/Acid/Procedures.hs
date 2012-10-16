{-# LANGUAGE TemplateHaskell, FlexibleContexts, TypeFamilies, DeriveDataTypeable #-}

module Core.Game.Acid.Procedures where

import Data.SafeCopy
import Data.Acid

import Core.Game.Acid.Procedures.Location
import Core.Game.Acid.Procedures.Lobby
import Core.Game.Acid.Procedures.Matchmaker
import Core.Game.Acid.Procedures.Room

import Core.Game.Acid.GameAcid

-- this will give duplicate constraint warnings
-- the template haskell generating the code is possibly buggy, needs to reduce constraint list to uniques
-- set vs list, people

dummy :: (SafeCopy (GameState p s), SafeCopy o) => Update (GameAcid p s o) ()
dummy = return ()

$(makeAcidic 
    ''GameAcid  
-- Location     -- tracks where user is within GameAcid
    [ 'setLocation, 'getLocation
-- Room         -- provides chat room service to lobby, matchmaker, and game
    , 'createRoom, 'send, 'receive, 'lookRooms
-- Lobby        -- where one waits to join games
    , 'getLobbyRoomId, 'getLobbyMemberIds
-- Matchmaker   -- where one waits for a game to fill up / begin
    , 'matchmakerAvailableCapacity, 'matchmakerHasCapacity, 'getMatchmakerOwner, 'getMatchmakerRoomId, 'getMatchmakerMemberIds, 'getMatchmakerLobbyId
-- Game         -- the fun part!
                -- nothing here yet; dummy just has needed constraints that game methods will provide later
    , 'dummy 
    ])
