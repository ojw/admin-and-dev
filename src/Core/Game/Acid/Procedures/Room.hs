{-# LANGUAGE DeriveDataTypeable, FlexibleContexts, GeneralizedNewtypeDeriving
  , MultiParamTypeClasses, OverloadedStrings, ScopedTypeVariables
  , TemplateHaskell, TypeFamilies, FlexibleInstances, RecordWildCards
  , TypeOperators #-}

module Core.Game.Acid.Procedures.Room

where

import Control.Category             ( (.) )
import Control.Monad.Reader         ( ask )
import Control.Monad.State          ( get, put )
import Data.SafeCopy                ( SafeCopy, base, deriveSafeCopy )
import Data.Lens.Template           ( makeLens )
import Data.Acid                    ( AcidState(..), EventState(..)
                                    , EventResult(..) , Query(..)
                                    , QueryEvent(..), Update(..)
                                    , UpdateEvent(..), IsAcidic(..), makeAcidic
                                    , openLocalState)
import Data.Data                    ( Data, Typeable )
import Data.IxSet                   ( Indexable(..), IxSet(..), (@=), Proxy(..)
                                    , getOne, ixFun, updateIx, size, null
                                    , ixSet, toList )
import Data.Lens                    ( (%=), (!=), (^$), (^=), Lens, (^%=) )
import Data.Lens.IxSet              ( ixLens )
import Data.Text                    ( Text )
import Prelude  hiding              ( null, (.) )

import Core.Auth.Auth               ( UserId )
import Core.Game.Acid.Types.Room

import Core.Game.Acid.GameAcid

room :: (Typeable key) => key -> Lens (IxSet Room) (Maybe Room)
room = ixLens          

addChat :: UserId -> Text -> Room -> Room
addChat uid msg rm = (chat ^%= (Chat (uid, msg) : )) rm

blankRoom :: RoomId -> Room
blankRoom rid = Room rid []

modRoom :: RoomId -> (Room -> Room) -> Update (GameAcid p s o) RoomState
modRoom rid fn = roomState %= (rooms ^%= (room rid ^%= fmap fn))

createRoom :: Update (GameAcid p s o) RoomId
createRoom =
    do  gameAcid <- get
        let next = nextRoomId ^$ (roomState ^$ gameAcid)
        roomState %= (rooms ^%= updateIx next (blankRoom next))
        roomState %= (nextRoomId ^%= succ)
        return next

send :: UserId -> RoomId -> Text -> Update (GameAcid p s o) RoomState
send userId roomId message = modRoom roomId (addChat userId message)

-- here a non-existent room simply return no chat
-- maybe return type should be Maybe [Chat]
receive :: UserId -> RoomId -> Query (GameAcid p s o) [Chat]
receive userId roomId =
    do  gameAcid <- ask
        case getOne $ (rooms . roomState ^$ gameAcid) @= roomId of
             Nothing    -> return []
             Just rm    -> return $ chat ^$ rm

lookRooms :: Query (GameAcid p s o) [Room]
lookRooms =
    do  gameAcid <- ask
        return $ toList $ rooms . roomState ^$ gameAcid 
