

module Server.Game.Acid.Procedures.Location

where

import Control.Applicative
import Control.Monad.Reader         
import Control.Monad.State 
import Data.IxSet
import Data.Acid
import Data.SafeCopy
import Data.Data
import Data.Lens

import Server.Auth.Acid        ( UserId )
import Server.Game.Acid.Types.Location
import Server.Game.Acid.Types.Room
import Server.Game.Acid.Types.Matchmaker
import Server.Game.Acid.GameAcid
import Server.Game.Acid.Procedures.Lobby
import Server.Game.Acid.Procedures.Matchmaker
import Server.Game.Acid.Procedures.Game

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
    ::  (Ord s, Ord p, Ord o, Typeable s, Typeable p, Typeable o)
    =>  UserId -> Query (GameAcid p s o) (Maybe RoomId)
getRoomId userId = do
    gameAcid <- ask
    case getLocation' userId $ gameAcid ^. locationState of
        Just (InGame gameId)    -> getGameRoomId gameId
        Just (InMatchmaker mId) -> getMatchmakerRoomId mId
        Just (InLobby lobbyId)  -> getLobbyRoomId lobbyId
        _                       -> return Nothing

-- this probably belongs in Matchmaker, but it pertains to leaving a Matchmaker and avoids circular imports by living here
deleteMatchmaker :: MatchmakerId -> Update (GameAcid p s o) ()
deleteMatchmaker matchmakerId = do
    gameAcid <- get
    case fmap _lobbyId $ getOne $ ((gameAcid ^. matchmakerState) ^. matchmakers) @= matchmakerId of
        Nothing     -> return ()
        Just lobby  -> case getMatchmakerMemberIds' matchmakerId gameAcid of
                        Nothing         -> return ()
                        Just players    ->mapM_ (\player -> setLocation player (Just (InLobby lobby))) players

leaveLocation :: UserId -> Update (GameAcid p s o) ()
leaveLocation userId = do
    gameAcid <- get
    case getLocation' userId $ gameAcid ^. locationState of
        Just (InMatchmaker mId) -> deleteMatchmaker mId
        _                       -> return ()
