{-# LANGUAGE TemplateHaskell, FlexibleContexts, TypeFamilies, DeriveDataTypeable, UndecidableInstances #-}

module Server.Game.Acid.Procedures where

import Data.SafeCopy
import Data.Acid

import Server.Game.Acid.Procedures.Location
import Server.Game.Acid.Procedures.Lobby
import Server.Game.Acid.Procedures.Matchmaker
import Server.Game.Acid.Procedures.Game

import Server.Game.Acid.GameAcid

-- this will give duplicate constraint warnings
-- the template haskell generating the code is possibly buggy, needs to reduce constraint list to uniques
-- set vs list, people


$(makeAcidic 
    ''GameAcid  
-- Location     -- tracks where user is within GameAcid
    [ 'setLocation, 'getLocation, 'deleteMatchmaker
-- Lobby        -- where one waits to join games
    , 'getLobbyMemberIds, 'lookLobbies, 'getLobbyName
-- Matchmaker   -- where one waits for a game to fill up / begin
    , 'matchmakerAvailableCapacity, 'matchmakerHasCapacity, 'getMatchmakerOwner, 'getMatchmakerMemberIds, 'getMatchmakerLobbyId, 'lookMatchmakers, 'getMatchmakerCapacity
-- Game         -- the fun part!
                -- nothing here yet; dummy just has needed constraints that game methods will provide later
    , 'dummy 
    ])
