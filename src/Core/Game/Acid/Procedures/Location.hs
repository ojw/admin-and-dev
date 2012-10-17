

module Core.Game.Acid.Procedures.Location

where

import Control.Applicative hiding (empty)
import Control.Monad.Reader 
import Data.IxSet
import Data.Acid
import Data.SafeCopy
import Data.Data
import Data.Lens
import Data.Lens.Template
import Data.Text hiding (empty)
import Data.ByteString.Lazy as L hiding (empty)

import Core.Auth.Acid        ( UserId )
import Core.Game.Acid.Types.Location
import Core.Game.Acid.Types.Room
import Core.Game.Acid.GameAcid
import Core.Game.Acid.Procedures.Lobby
import Core.Game.Acid.Procedures.Matchmaker
import Core.Game.Acid.Procedures.Game

getLocation' :: UserId -> LocationState -> Maybe Location
getLocation' userId locationState =
    case getOne $ (locations ^$ locationState) @= userId of
        Nothing         -> Nothing
        Just userLocation  -> location ^$ userLocation

setLocation' :: UserId -> Maybe Location -> LocationState -> LocationState
setLocation' userId mLocation locationState =
    locations ^%= updateIx userId (UserLocation userId mLocation) $ locationState

getLocation :: UserId -> Query (GameAcid p s o) (Maybe Location)
getLocation userId = do
    gameAcid <- ask
    return $ getLocation' userId $ gameAcid ^. locationState

setLocation :: UserId -> Maybe Location -> Update (GameAcid p s o) LocationState
setLocation userId mLocation = do
    locationState %= setLocation' userId mLocation

getRoomId 
    ::  (Ord s, Ord p, Typeable s, Typeable p) 
    =>  UserId -> Query (GameAcid p s o) (Maybe RoomId)
getRoomId userId = do
    gameAcid <- ask
    case getLocation' userId $ gameAcid ^. locationState of
        Just (InGame gameId)    -> getGameRoomId gameId
        Just (InMatchmaker mId) -> getMatchmakerRoomId mId
        Just (InLobby lobbyId)  -> getLobbyRoomId lobbyId
        _                       -> return Nothing
