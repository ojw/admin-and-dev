{-# LANGUAGE DeriveDataTypeable, FlexibleContexts, GeneralizedNewtypeDeriving
  , MultiParamTypeClasses, OverloadedStrings, ScopedTypeVariables, TemplateHaskell
  , TypeFamilies, FlexibleInstances, RecordWildCards #-}

module Authentication 

( addUser
, validateLogin
, validateSession
) where

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
import System.Random        ( StdGen )

newtype UserId = UserId { _unUserId :: Integer }
    deriving (Eq, Ord, Data, Enum, Typeable, SafeCopy, Read, Show)

-- using newtypes makes the code cumbersome but prevents using Password instead of PasswordHash
-- and vice versa :/
type Password = ByteString
type PasswordHash = ByteString
type PasswordMap = Map UserId PasswordHash

type SessionId = Integer
type SessionMap = Map UserId SessionId

data AuthenticationState = AuthenticationState
    { _passwordMap  :: PasswordMap
    , _sessionMap   :: SessionMap
    , _rngString    :: String
    }
    deriving (Eq, Ord, Typeable)

$(makeLens ''AuthenticationState)
$(deriveSafeCopy 0 'base ''AuthenticationState)

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
