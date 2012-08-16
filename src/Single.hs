{-# LANGUAGE DeriveDataTypeable, FlexibleContexts, GeneralizedNewtypeDeriving
  , MultiParamTypeClasses, OverloadedStrings, ScopedTypeVariables, TemplateHaskell
  , TypeFamilies, FlexibleInstances, RecordWildCards, TypeOperators #-}

module Single 

where

import Control.Applicative          ( (<$>), (<*>) )
import Control.Category             ( (.) )
import Control.Exception.Lifted     ( bracket)
import Control.Monad                ( msum, liftM )
import Control.Monad.Reader         ( ask, ReaderT )
import Control.Monad.State          ( get, put, gets )
import Control.Monad.Trans          ( lift, MonadIO(..) )
import Control.Monad.Trans.Control  ( MonadBaseControl )
import Crypto.BCrypt                ( validatePassword, hashPasswordUsingPolicy
                                    , slowerBcryptHashingPolicy )
import Data.Maybe                   ( fromMaybe, fromJust )
import Data.SafeCopy                ( SafeCopy, base, deriveSafeCopy )
import Data.Lens.Template           ( makeLens )
import Data.Acid                    ( AcidState(..), EventState(..)
                                    , EventResult(..) , Query(..)
                                    , QueryEvent(..), Update(..)
                                    , UpdateEvent(..), IsAcidic(..), makeAcidic
                                    , openLocalState)
import Data.Acid.Advanced           ( query', update' )
import Data.Acid.Local              ( createCheckpointAndClose 
                                    , openLocalStateFrom)
import Data.ByteString              ( ByteString, pack )
import Data.Data                    ( Data, Typeable )
import Data.Functor                 ( (<$>) )
import Data.IxSet                   ( Indexable(..), IxSet(..), (@=), Proxy(..)
                                    , getOne, ixFun, updateIx, size, null
                                    , ixSet )
import Data.Lens                    ( (%=), (!=), (^$), (^=), Lens )
import Data.Lens.IxSet              ( ixLens )
import Data.Map                     ( Map, insert, adjust, lookup )
import Data.Text                    ( Text, unpack, reverse, toUpper )
import qualified Data.Text as Text
import Data.Text.Encoding           ( encodeUtf8 )
import Happstack.Server.RqData
import Happstack.Server             ( Response, ServerPart, ServerPartT, ok
                                    , toResponse, simpleHTTP, nullConf
                                    , seeOther, dir, notFound, seeOther
                                    , Method(..), methodM)
import Prelude  hiding              ( null, (.) )
import Text.Boomerang.TH            ( derivePrinterParsers )
import Web.Routes                   ( RouteT, runRouteT, Site(..), setDefault
                                    , MonadRoute, askRouteFn, URL, PathInfo )
import Web.Routes.Boomerang         ( (<>), lit, (</>), anyText, (:-), Router
                                    , xmaph, int, boomerangSite, integer )
import Web.Routes.Happstack         ( implSite )
import Web.Routes.TH                ( derivePathInfo )


data User = User
    { _userId       :: UserId
    , _email        :: Email
    , _name         :: Name
    , _password     :: Password
    , _sessionId    :: Maybe SessionId
    } deriving (Eq, Ord, Typeable)

newtype UserId = UserId { _unUserId :: Integer } deriving (Eq, Ord, Enum, Data, Typeable, SafeCopy, Read, Show)
newtype SessionId = SessionId Integer deriving (Eq, Ord, Enum, Data, Typeable, SafeCopy)
newtype Email = Email Text deriving (Eq, Ord, Data, Typeable, SafeCopy)
newtype Name = Name Text deriving (Eq, Ord, Data, Typeable, SafeCopy)
newtype Password = Password { unPassword :: ByteString } deriving (Eq, Ord, Data, Typeable, SafeCopy)

$(makeLens ''User)
$(deriveSafeCopy 0 'base ''User)

instance Indexable User where
    empty = ixSet [ ixFun $ \user -> [ userId  ^$ user ]
                  , ixFun $ \user -> [ email ^$ user  ]
                  , ixFun $ \user -> [ name ^$ user ]
                  ]

data AuthenticationState = AuthenticationState
    { _users            :: IxSet User
    , _nextUserId       :: UserId
    , _nextSessionId    :: SessionId
    } deriving (Eq, Ord, Typeable)

$(makeLens ''AuthenticationState)
$(deriveSafeCopy 0 'base ''AuthenticationState)

checkEmailAvailability :: Text -> Query AuthenticationState Bool
checkEmailAvailability newEmail = 
    do authState <- ask
       return $ null $ (users ^$ authState) @= (Email newEmail)

checkNameAvailability :: Text -> Query AuthenticationState Bool
checkNameAvailability newName =
    do authState <- ask
       return $ null $ (users ^$ authState) @= (Name newName)

addUser_ :: Email -> Name -> Password -> Update AuthenticationState UserId
addUser_ e n p = do  AuthenticationState{..} <- get
                     users %= updateIx _nextUserId (User _nextUserId e n p Nothing)
                     nextUserId %= succ

getUser :: Text -> Query AuthenticationState (Maybe User)
getUser t =
    do  authState <- ask
        let byName  = getOne $ (users ^$ authState) @= (Name t)
            byEmail = getOne $ (users ^$ authState) @= (Email t)
         in
            return $ msum [byName, byEmail]

validateLogin :: User -> Text -> Bool
validateLogin usr pw = validatePassword (unPassword (password ^$ usr)) (encodeUtf8 pw)

validateSession :: User -> SessionId -> Bool
validateSession usr sn = case sessionId ^$ usr of
                              Nothing       -> False
                              (Just sid)    -> sid == sn

createSession :: UserId -> Update AuthenticationState SessionId
createSession uid = 
    do authState <- get
       let usr = fromJust $ getOne $ (users ^$ authState) @= uid
           usr' = (sessionId ^= Just (nextSessionId ^$ authState)) usr
       users %= updateIx uid usr'
       nextSessionId %= succ
       
hashPassword :: ByteString -> IO Password
hashPassword pwd = hashPasswordUsingPolicy slowerBcryptHashingPolicy pwd >>= (return . Password . fromJust)

addUser :: (MonadIO m) => Text -> Text -> Text -> m (Update AuthenticationState UserId)
addUser e n p =
    do passwordHash <- liftIO $ hashPassword $ encodeUtf8 p
       return $ addUser_ (Email e) (Name n) passwordHash 

getUserById :: UserId -> Query AuthenticationState (Maybe User)
getUserById uid =
    do  authState <- ask
        return $ getOne $ (users ^$ authState) @= uid
               

$(makeAcidic ''AuthenticationState ['getUser, 'getUserById])

getLoginCredentials :: ServerPart (Text, Text)
getLoginCredentials =
    do  nameOrEmail <- lookRead "nameOrEmail"
        password    <- lookRead "password"
        return (nameOrEmail, password)

getSessionCookie :: ServerPart (UserId, SessionId)
getSessionCookie =
    do  (uid :: Integer) <- readCookieValue "userId"
        (sid :: Integer) <- readCookieValue "sessionId"
        return (UserId uid, SessionId sid)

tryLogIn :: AcidState AuthenticationState -> ServerPart Bool
tryLogIn acid =
    do  (nameOrEmail, password) <- getLoginCredentials
        maybeUser <- query' acid (GetUser nameOrEmail)
        case maybeUser of
             Nothing    -> return False
             Just usr   -> return $ validateLogin usr password 

loggedInUser :: AcidState AuthenticationState -> ServerPart (Maybe UserId)
loggedInUser acid =
    do  (uid, sid)  <- getSessionCookie
        maybeUser   <- query' acid (GetUserById uid)
        case maybeUser of
             Nothing    -> return Nothing
             Just usr   -> if validateSession usr sid then return (Just uid) else return Nothing

loggedInUser' :: App (Maybe UserId)
loggedInUser' =
    do acid <- ask
       return $ Just (UserId 1)

type App a = ServerPartT (ReaderT (AcidState AuthenticationState) IO) a

withLoggedInUser :: AcidState AuthenticationState -> a -> (UserId -> a) -> ServerPart a
withLoggedInUser acid failure success = 
    do  maybeUserId <- loggedInUser acid
        return $ maybe failure success maybeUserId

withLoggedInUser' :: a -> (UserId -> a) -> App a
withLoggedInUser' failure success =
    do maybeUserId <- loggedInUser'
       return $ maybe failure success maybeUserId

------------------------------------------------------------

data Sitemap
    = Home
    | Profile UserId
    | Echo Text
      deriving (Eq, Ord, Read, Show, Data, Typeable)

$(derivePrinterParsers ''Sitemap)

sitemap :: Router () (Sitemap :- ())
sitemap =
    (  rHome
    <> rProfile . (lit "profile" </> userId)
    <> rEcho . (lit "message" </> anyText)
    )
    where userId :: Router () (UserId :- ())
          userId = 
              xmaph UserId (Just . _unUserId) integer


------------------------------------------------------------


route :: Sitemap -> RouteT Sitemap (ServerPartT IO) Response
route url =
    case url of
      Home              -> ok $ toResponse $ ("bla" :: Text)
      (Profile userId)  -> ok $ toResponse $ "Profile" ++ show (_unUserId userId)
      (Echo message)    -> ok $ toResponse $ "Message" ++ unpack message

site :: Site Sitemap (ServerPartT IO Response)
site =
       setDefault Home $ boomerangSite (runRouteT route) sitemap

main :: IO ()
main = simpleHTTP nullConf $
       msum [ dir "favicon.ico" $ notFound (toResponse ())
            , implSite "http://localhost:8000" "/route" site
            , seeOther ("/route/" :: String) (toResponse ())
            ] 

-- Room

newtype RoomId = RoomId { _unRoomId :: Integer } deriving (Eq, Ord, Enum, Data, Typeable, SafeCopy, Read, Show)

$(makeLens ''RoomId)

data Room = Room
    { _roomId :: RoomId
    , _capacity :: Int
    , _members :: [UserId]
    , _chat :: [(UserId, Text)]
    } deriving (Eq, Ord, Data, Typeable)

$(makeLens ''Room)
$(deriveSafeCopy 0 'base ''Room)

instance Indexable Room where
    empty = ixSet [ ixFun $ \room -> [ roomId ^$ room ]
                  , ixFun $ \room -> [ capacity ^$ room ]
                  , ixFun $ \room -> members ^$ room
                  ]

data RoomState = RoomState
    { _nextRoomId   :: RoomId
    , _rooms        :: IxSet Room
    }
    deriving (Eq, Ord, Typeable)

$(makeLens ''RoomState)
$(deriveSafeCopy 0 'base ''RoomState)

initialRoomState :: RoomState
initialRoomState = RoomState
    { _nextRoomId = RoomId 1
    , _rooms      = empty
    }

$(makeAcidic ''RoomState []) 
