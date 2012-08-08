{-# LANGUAGE DeriveDataTypeable, FlexibleContexts, GeneralizedNewtypeDeriving
  , MultiParamTypeClasses, OverloadedStrings, ScopedTypeVariables, TemplateHaskell
  , TypeFamilies, FlexibleInstances, RecordWildCards #-}

module Authentication 

where

import Data.Functor      ((<$>))
import System.FilePath      ((</>))
import Control.Monad.State (get, put, gets)
import Control.Monad.Reader (ask)
import Data.Maybe (fromMaybe, fromJust)
import Data.Text (Text)
import qualified Data.Text as Text
import Data.SafeCopy        ( SafeCopy, base, deriveSafeCopy )
import Data.Data            ( Data, Typeable )
import Data.Lens            ( (%=), (!=), (^$) )
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

import Prelude  hiding      (null)
import Data.Map             (Map, insert, adjust, lookup)
import Data.ByteString      ( ByteString, pack )
import Crypto.BCrypt        ( validatePassword, hashPasswordUsingPolicy
                            , slowerBcryptHashingPolicy )
import System.Random        ( StdGen )


data User = User
    { _userId       :: UserId
    , _email        :: Email
    , _name         :: Name
    , _password     :: Password
    , _sessionId    :: Maybe SessionId
    } deriving (Eq, Ord, Typeable)

newtype UserId = UserId Integer deriving (Eq, Ord, Enum, Data, Typeable, SafeCopy)
newtype SessionId = SessionId Integer deriving (Eq, Ord, Data, Typeable, SafeCopy)
newtype Email = Email Text deriving (Eq, Ord, Data, Typeable, SafeCopy)
newtype Name = Name Text deriving (Eq, Ord, Data, Typeable, SafeCopy)
newtype Password = Password ByteString deriving (Eq, Ord, Data, Typeable, SafeCopy)

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
$(makeAcidic ''AuthenticationState [])

checkEmailAvailability :: Text -> Query AuthenticationState Bool
checkEmailAvailability newEmail = 
    do authState <- ask
       return $ null $ (users ^$ authState) @= (Email newEmail)

checkNameAvailability :: Text -> Query AuthenticationState Bool
checkNameAvailability newName =
    do authState <- ask
       return $ null $ (users ^$ authState) @= (Name newName)

addUser :: Email -> Name -> Password -> Update AuthenticationState UserId
addUser e n p = do  AuthenticationState{..} <- get
                    users %= updateIx _nextUserId (User _nextUserId e n p Nothing)
                    nextUserId %= succ

hashPassword :: ByteString -> IO Password
hashPassword pwd = hashPasswordUsingPolicy slowerBcryptHashingPolicy pwd >>= (return . Password . fromJust)




{-
-- internal only
updatePassword' :: UserId -> Password -> Update AuthenticationState PasswordMap
updatePassword' uid pwd = passwordMap %= adjust (\p -> pwd) uid
     
-- internal only
updateSession' :: UserId -> SessionId -> Update AuthenticationState SessionMap
updateSession' uid sid = sessionMap %= adjust (\s -> sid) uid

-- internal only
validateLogin' :: UserId -> Password -> PasswordMap -> Bool
validateLogin' uid pwd pwdMap =
    case lookup uid pwdMap of
         Nothing        -> False
         (Just pwdHash) -> validatePassword pwdHash pwd

-- internal only
addUser' :: UserId -> PasswordHash -> Update AuthenticationState PasswordMap
addUser' uid pwdh = passwordMap %= insert uid pwdh

addUser :: UserId -> Password -> IO (Update AuthenticationState PasswordMap)
addUser uid pwd = do pwdh <- hash pwd
                     return $ addUser' uid pwdh

validateLogin :: UserId -> Password -> Update AuthenticationState (Maybe SessionId)
validateLogin uid pwd =
    do (AuthenticationState pm sm _) <- get
       if validateLogin' uid pwd pm 
       then (updateSession' uid 1) >> (return $ Just 1)
       else return Nothing

validateSession :: Maybe UserId -> Maybe SessionId -> Query AuthenticationState (Maybe UserId)
validateSession _ Nothing = return Nothing
validateSession Nothing _ = return Nothing
validateSession (Just uid) (Just sid) = 
    do (AuthenticationState pm sm _) <- ask
       case lookup uid sm of
            Nothing     -> return Nothing
            (Just sid)  -> return $ Just uid

-- suppressing Maybe is clearly a mistake
hash :: Password -> IO PasswordHash
hash pwd = hashPasswordUsingPolicy slowerBcryptHashingPolicy pwd >>= (return . fromJust)

$(makeAcidic ''AuthenticationState ['addUser', 'validateSession, 'validateLogin])

-}
