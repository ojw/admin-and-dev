{-# LANGUAGE DeriveDataTypeable, FlexibleContexts, GeneralizedNewtypeDeriving
  , MultiParamTypeClasses, OverloadedStrings, ScopedTypeVariables
  , TemplateHaskell, TypeFamilies, FlexibleInstances, RecordWildCards
  , TypeOperators #-}

module Single 

where

import Control.Applicative          ( (<$>), (<*>), Applicative, Alternative )
import Control.Category             ( (.) )
import Control.Exception.Lifted     ( bracket)
import Control.Monad                ( msum, liftM, MonadPlus )
import Control.Monad.Reader         ( ask, ReaderT(..), MonadReader )
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
                                    , Method(..), methodM, FilterMonad
                                    , WebMonad, ServerMonad, Happstack
                                    , mapServerPartT )
import Prelude  hiding              ( null, (.) )
import qualified System.FilePath as FP
import Text.Boomerang.TH            ( derivePrinterParsers )
import Web.Routes                   ( RouteT, runRouteT, Site(..), setDefault
                                    , MonadRoute, askRouteFn, URL, PathInfo )
import Web.Routes.Boomerang         ( (<>), lit, (</>), anyText, (:-), Router
                                    , xmaph, int, boomerangSite, integer )
import Web.Routes.Happstack         ( implSite )
import Web.Routes.TH                ( derivePathInfo )

import Text.Blaze.Html5 hiding      ( base )
import Text.Blaze.Html5.Attributes hiding ( dir )
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A

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

initialAuthenticationState :: AuthenticationState
initialAuthenticationState = AuthenticationState
    { _users = empty
    , _nextUserId = UserId 1
    , _nextSessionId = SessionId 1
    }

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
{-
loggedInUser' :: App (Maybe UserId)
loggedInUser' =
    do acid <- ask
       return $ Just (UserId 1)
-}
--type App a = ServerPartT (ReaderT (AcidState AuthenticationState) IO) a

withLoggedInUser :: AcidState AuthenticationState -> a -> (UserId -> a) -> ServerPart a
withLoggedInUser acid failure success = 
    do  maybeUserId <- loggedInUser acid
        return $ maybe failure success maybeUserId
{-
withLoggedInUser' :: a -> (UserId -> a) -> App a
withLoggedInUser' failure success =
    do maybeUserId <- loggedInUser'
       return $ maybe failure success maybeUserId
-}
------------------------------------------------------------

data Sitemap
    = Home
    | Login
    | Register
    | Profile UserId
    | Echo Text
      deriving (Eq, Ord, Read, Show, Data, Typeable)

$(derivePrinterParsers ''Sitemap)

sitemap :: Router () (Sitemap :- ())
sitemap =
    (  rHome
    <> rLogin . (lit "login")
    <> rRegister . (lit "register")
    <> rProfile . (lit "profile" </> userId)
    <> rEcho . (lit "message" </> anyText)
    )
    where userId :: Router () (UserId :- ())
          userId = 
              xmaph UserId (Just . _unUserId) integer


------------------------------------------------------------

template :: String -> [H.Html] -> H.Html -> H.Html
template title headers body =
    H.docTypeHtml $ do
      H.head $ do
        H.title (H.toHtml title)
      H.body $ do
        body

loginBox :: H.Html
loginBox = H.form ! action "login" ! method "post" $ do
             H.toHtml ("name or email" :: String) 
             H.input ! type_ "text"
             H.br
             H.toHtml ("password" :: String)
             H.input ! type_ "password"
             H.br
             H.button ! type_ "submit" $ H.toHtml ("Log in." :: String)

registrationBox :: H.Html
registrationBox = 
    H.form ! action "register" ! method "post" $ do
      H.toHtml ("register" :: String)
      H.br >> H.toHtml ("user name" :: String) >> H.input ! type_ "text"
      H.br >> H.toHtml ("email" :: String) >> H.input ! type_ "text"
      H.br >> H.toHtml ("again" :: String) >> H.input ! type_ "text"
      H.br >> H.toHtml ("password" :: String) >> H.input ! type_ "password"
      H.br >> H.toHtml ("pw again" :: String) >> H.input ! type_ "password"
      
------------------------------------------------------------

route :: Sitemap -> RouteT Sitemap App Response -- (ServerPartT IO) Response
route url =
    case url of
      Home              -> ok $ toResponse $ template "Title" [] loginBox
      Login             -> ok $ toResponse $ ("Login attempted" :: String)
      Register          -> ok $ toResponse $ template "Register" [] registrationBox
      (Profile userId)  -> ok $ toResponse $ "Profile" ++ show (_unUserId userId)
      (Echo message)    -> ok $ toResponse $ "Message" ++ unpack message

site :: Site Sitemap (App Response) -- (ServerPartT IO Response)
site =
       setDefault Home $ boomerangSite (runRouteT route) sitemap

main :: IO ()
main = withAcid Nothing $ \acid -> simpleHTTP nullConf $ runApp acid serverPart

serverPart:: App Response -- ServerPartT IO Response
serverPart = msum [ dir "favicon.ico" $ notFound (toResponse ())
                  , implSite "http://localhost:8000" "/route" site
                  , seeOther ("/route/" :: String) (toResponse ())
                  ]

------------------------------------------------------------

class HasAcidState m st where
   getAcidState :: m (AcidState st)

query :: forall event m.
         ( Functor m
         , MonadIO m
         , QueryEvent event
         , HasAcidState m (EventState event)
         ) =>
         event
      -> m (EventResult event)
query event =
    do as <- getAcidState
       query' (as :: AcidState (EventState event)) event

update :: forall event m.
          ( Functor m
          , MonadIO m
          , UpdateEvent event
          , HasAcidState m (EventState event)
          ) =>
          event
       -> m (EventResult event)
update event =
    do as <- getAcidState
       update' (as :: AcidState (EventState event)) event

-- | bracket the opening and close of the `AcidState` handle.

-- automatically creates a checkpoint on close
withLocalState :: (MonadBaseControl IO m, MonadIO m, IsAcidic st, Typeable st) =>
                  Maybe FilePath        -- ^ path to state directory
               -> st                    -- ^ initial state value
               -> (AcidState st -> m a) -- ^ function which uses the `AcidState` handle
               -> m a
withLocalState mPath initialState =
    bracket (liftIO $ (maybe openLocalState openLocalStateFrom mPath) initialState)
            (liftIO . createCheckpointAndClose) 

data Acid = Acid { acidAuthState    :: AcidState AuthenticationState
                 }

withAcid :: Maybe FilePath -> (Acid -> IO a) -> IO a
withAcid mBasePath action =
    let basePath = fromMaybe "_state" mBasePath
    in withLocalState (Just $ basePath FP.</> "auth") initialAuthenticationState $ \c ->
           action (Acid c) 

newtype App a = App { unApp :: ServerPartT (ReaderT Acid IO) a }
    deriving ( Functor, Alternative, Applicative, Monad, MonadPlus, MonadIO
               , HasRqData, ServerMonad ,WebMonad Response, FilterMonad Response
               , Happstack, MonadReader Acid)

runApp :: Acid -> App a -> ServerPartT IO a
runApp acid (App sp) = mapServerPartT (flip runReaderT acid) sp


instance HasAcidState App AuthenticationState where
    getAcidState = acidAuthState    <$> ask

{-
main' :: IO ()
main' =
    withAcid Nothing $ \acid ->
        simpleHTTP nullConf $ runApp acid serverPart
-}
