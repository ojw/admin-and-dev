{-# LANGUAGE DeriveDataTypeable, FlexibleContexts, GeneralizedNewtypeDeriving
  , MultiParamTypeClasses, OverloadedStrings, ScopedTypeVariables, TemplateHaskell
  , TypeFamilies, FlexibleInstances, RecordWildCards #-}

module Authentication 

where

import Data.Functor      ((<$>))
import System.FilePath      ((</>))
import Control.Monad        ( msum, liftM )
import Control.Monad.State (get, put, gets)
import Control.Monad.Reader (ask)
import Data.Maybe (fromMaybe, fromJust)
import Data.Text (Text)
import qualified Data.Text as Text
import Data.SafeCopy        ( SafeCopy, base, deriveSafeCopy )
import Data.Data            ( Data, Typeable )
import Data.Lens            ( (%=), (!=), (^$), (^=) )
import Data.Lens.Template   ( makeLens )
import Data.Acid            ( AcidState(..), EventState(..), EventResult(..)
                            , Query(..), QueryEvent(..), Update(..), UpdateEvent(..)
                            , IsAcidic(..), makeAcidic, openLocalState
                            )
import Data.Acid.Local      ( createCheckpointAndClose
                            , openLocalStateFrom
                            )
import Data.Acid.Advanced   ( query', update' )
-- import Data.Lens.IxSet      (ixLens)
import Control.Exception.Lifted    (bracket)
import Control.Monad.Trans.Control (MonadBaseControl)
import Control.Monad.Trans  (MonadIO(..))

import Data.IxSet           ( Indexable(..), IxSet(..), (@=), Proxy(..), getOne
                            , ixFun, ixSet, updateIx, size, null )

import Prelude  hiding      (null, (.))
import Data.Map             (Map, insert, adjust, lookup)
import Data.ByteString      ( ByteString, pack )
import Crypto.BCrypt        ( validatePassword, hashPasswordUsingPolicy
                            , slowerBcryptHashingPolicy )
import System.Random        ( StdGen )

import Data.Text.Encoding   ( encodeUtf8 )
import Control.Category     ( (.) )

import Happstack.Server

import qualified Text.Blaze.Html4.Strict as H

data User = User
    { _userId       :: UserId
    , _email        :: Email
    , _name         :: Name
    , _password     :: Password
    , _sessionId    :: Maybe SessionId
    } deriving (Eq, Ord, Typeable)

newtype UserId = UserId Integer deriving (Eq, Ord, Enum, Data, Typeable, SafeCopy)
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

{-
validateLogin :: Text -> Text -> Query AuthenticationState (Either Text UserId)
validateLogin nameOrEmail pw =
    do maybeUser <- getUser nameOrEmail
       case maybeUser of
            Nothing     -> return $ Left "Invalid username / email address."
            Just usr    -> case validatePassword (unPassword (password ^$ usr)) (encodeUtf8 pw) of
                                True    -> return $ Right (userId ^$ usr)
                                False   -> return $ Left "Invalid password."
-}

validateLogin :: User -> Text -> Bool
validateLogin usr pw = validatePassword (unPassword (password ^$ usr)) (encodeUtf8 pw)

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

$(makeAcidic ''AuthenticationState ['getUser])

getCredentials :: RqData (Text, Text)
getCredentials =
    do  nameOrEmail <- look "nameOrEmail"
        password    <- look "password"
        return (Text.pack nameOrEmail, Text.pack password)

-- horribly wrong, just a test
tryTest :: AcidState AuthenticationState -> ServerPart String
tryTest acid =
    do foo <- look "foo"
       stuff <- query' acid (GetUser (Text.pack foo))
       return "ASDF"
{-
tryLogIn :: AcidState AuthenticationState -> ServerPart Response
tryLogIn acid =
    do  (nameOrEmail, password) <- getCredentials
        maybeUser <- query' acid (GetUser nameOrEmail)
        case maybeUser of
             Nothing    -> ok $ "FOO"
             Just usr   -> ok $ show $ validateLogin usr password
-}
