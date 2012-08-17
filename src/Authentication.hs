{-# LANGUAGE DeriveDataTypeable, FlexibleContexts, GeneralizedNewtypeDeriving
  , MultiParamTypeClasses, OverloadedStrings, ScopedTypeVariables
  , TemplateHaskell, TypeFamilies, FlexibleInstances, RecordWildCards
  , TypeOperators #-}

module Authentication 

where

import Control.Applicative          ( (<$>), (<*>), Applicative, Alternative )
import Control.Category             ( (.) )
import Control.Monad                ( msum, liftM, MonadPlus )
import Control.Monad.Reader         ( ask, ReaderT(..), MonadReader )
import Control.Monad.State          ( get, put, gets )
import Control.Monad.Trans          ( lift, MonadIO(..) )
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

import HasAcidState

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

loggedInUser' :: (HasAcidState m AuthenticationState, Monad m) => m (Maybe UserId)
loggedInUser' =
    do return $ Just (UserId 1)

withLoggedInUser :: AcidState AuthenticationState -> a -> (UserId -> a) -> ServerPart a
withLoggedInUser acid failure success = 
    do  maybeUserId <- loggedInUser acid
        return $ maybe failure success maybeUserId

withLoggedInUser' :: (HasAcidState m AuthenticationState, Monad m) => a -> (UserId -> a) -> m a
withLoggedInUser' failure success =
    do maybeUserId <- loggedInUser'
       return $ maybe failure success maybeUserId

------------------------------------------------------------

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

data AuthSiteMap
    = Login
    | Registration
    | Register
      deriving (Eq, Ord, Read, Show, Data, Typeable)

$(derivePrinterParsers ''AuthSiteMap)

authSiteMap :: Router () (AuthSiteMap :- ())
authSiteMap =
    (  rLogin . (lit "login")
    <> rRegistration . (lit "registration")
    <> rRegister . (lit "register")
    )

authRoute :: (HasAcidState m AuthenticationState, FilterMonad Response m) 
          => (String -> [H.Html] -> H.Html -> H.Html) -> AuthSiteMap -> RouteT AuthSiteMap m Response
authRoute template url =
    case url of
      Login             -> ok $ toResponse $ template "Login" [] $ H.toHtml ("Login attempted" :: String)
      Registration       -> ok $ toResponse $ template "Registration" [] registrationBox
      Register          -> ok $ toResponse $ template "Register" [] $ H.toHtml ("Registration succeeded or failed here." :: String)

authSite :: (HasAcidState m AuthenticationState, FilterMonad Response m) 
         => (String -> [H.Html] -> H.Html -> H.Html) -> Site AuthSiteMap (m Response)
authSite template = boomerangSite (runRouteT (authRoute template)) authSiteMap
