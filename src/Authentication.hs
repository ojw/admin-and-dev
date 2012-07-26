{-# LANGUAGE DeriveDataTypeable, FlexibleContexts, GeneralizedNewtypeDeriving
  , MultiParamTypeClasses, OverloadedStrings, ScopedTypeVariables, TemplateHaskell
  , TypeFamilies, FlexibleInstances, RecordWildCards #-}

module Authentication where

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

import Prelude  hiding      (lookup)
import Data.Map             (Map, insert, adjust, lookup)
import Data.ByteString      ( ByteString, pack )
import Crypto.BCrypt        ( validatePassword, hashPasswordUsingPolicy
                            , slowerBcryptHashingPolicy )

newtype UserId = UserId { _unUserId :: Integer }
    deriving (Eq, Ord, Data, Enum, Typeable, SafeCopy, Read, Show)

type Password = ByteString

type PasswordHash = ByteString

newtype SessionId = SessionId { _unSessionId :: Integer }
    deriving (Eq, Ord, Data, Typeable, SafeCopy)

type PasswordMap = Map UserId PasswordHash

newtype SessionMap  = SessionMap { _unSessionMap :: Map UserId SessionId }
    deriving (Eq, Ord, Data, Typeable, SafeCopy)

$(makeLens ''SessionMap)

data AuthenticationState = AuthenticationState
    { _passwordMap   :: PasswordMap
    , _sessionMap    :: SessionMap
    }
    deriving (Eq, Ord, Typeable)

$(makeLens ''AuthenticationState)
$(deriveSafeCopy 0 'base ''AuthenticationState)

-- don't think this is necessary but I wrote it before realizing that
userExists :: UserId -> Query AuthenticationState Bool
userExists uid = do (AuthenticationState pm sm) <- ask
                    case lookup uid pm of
                         Nothing   -> return False
                         (Just _)  -> return True

addUser :: UserId -> Password -> Update AuthenticationState PasswordMap
addUser uid pwd = 
    do (AuthenticationState pm sm) <- get
       passwordMap != insert uid pwd pm

-- internal only
updatePassword' :: UserId -> Password -> Update AuthenticationState PasswordMap
updatePassword' uid pwd =
    do (AuthenticationState pm sm) <- get
       passwordMap != adjust (\p -> pwd) uid pm
     
-- internal only
updateSession' :: UserId -> SessionId -> Update AuthenticationState SessionMap
updateSession' uid sid = 
    do (AuthenticationState pm sm) <- get
       sessionMap != SessionMap (adjust (\s -> sid) uid (_unSessionMap sm))

validateSession :: UserId -> Maybe SessionId -> Update AuthenticationState Bool
validateSession uid Nothing = return False
validateSession uid sid = 
    do (AuthenticationState pm sm) <- get
       case lookup uid (_unSessionMap sm) of
            Nothing   -> return False
            sid       -> return True

validateLogin :: UserId -> Password -> Update AuthenticationState (Maybe SessionId)
validateLogin uid pwd =
    do (AuthenticationState pm sm) <- get
       if validateLogin' uid pwd pm then return (Just (SessionId 1)) else return Nothing

validateLogin' :: UserId -> Password -> PasswordMap -> Bool
validateLogin' uid pwd pwdMap =
    case lookup uid pwdMap of
         Nothing        -> False
         (Just pwdHash) -> validatePassword pwdHash pwd

hash :: Password -> IO (Maybe PasswordHash)
hash pwd = hashPasswordUsingPolicy slowerBcryptHashingPolicy pwd

$(makeAcidic ''AuthenticationState ['addUser, 'validateSession, 'validateLogin]) 
