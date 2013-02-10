{-# LANGUAGE FlexibleContexts, OverloadedStrings, GeneralizedNewtypeDeriving #-}

module Framework.Auth.Internal.Api where

import Data.Functor ( (<$>) )
import Data.Maybe ( isJust, fromJust )
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Error
import Control.Monad.RWS
import Data.Text ( Text )
import Data.Lens
import Data.IxSet
import Data.ByteString.Char8 ( ByteString )
import Framework.Profile
import Framework.Auth.Internal.Types.UserPassword
import Framework.Auth.Internal.Types.UserToken
import Framework.Auth.Internal.Types.AuthState
import Framework.Auth.Internal.Types.Error
import Crypto.BCrypt
import System.Entropy
import Data.Monoid

data AuthApi
    = Register UserName Email PlainPass
    | LogIn Text PlainPass
    | UpdatePassword Text PlainPass PlainPass
    | LogOut Text

data AuthView
    = AuthTokenView AuthToken
    | AuthViewSuccess Bool

class (Functor m, Monad m, MonadReader ProfileState m, MonadState AuthState m, MonadError AuthError m) => MonadAuthAction m

newtype AuthAction a = AuthAction { unAuthAction :: RWST ProfileState Text AuthState (ErrorT AuthError IO) a}
    deriving (Functor, Monad, MonadReader ProfileState, MonadState AuthState, MonadError AuthError)

instance MonadAuthAction AuthAction

runAuthAction :: AuthAction a -> ProfileState -> AuthState -> IO (Either AuthError (a, AuthState, Text))
runAuthAction (AuthAction action) profileState authState = do
    runErrorT $ (runRWST action) profileState authState

runAuthApi :: (MonadIO m, MonadAuthAction m, MonadState ProfileState m) => AuthApi -> m AuthView
runAuthApi (Register userName email plainPass) = register userName email plainPass >> return (AuthViewSuccess True)
runAuthApi (LogIn text plainPass) = logIn text plainPass >> return (AuthViewSuccess True)
runAuthApi (UpdatePassword text oldPass newPass) = updatePassword text oldPass newPass >> return (AuthViewSuccess True)
runAuthApi (LogOut text) = logOut text >> return (AuthViewSuccess True)

isUserNameAvailable :: (Functor m, MonadReader ProfileState m) => UserName -> m Bool
isUserNameAvailable email = (not . isJust) <$> lookupUserIdByUserName email

isEmailAvailable :: (Functor m, MonadReader ProfileState m) => Email -> m Bool
isEmailAvailable email = (not . isJust) <$> lookupUserIdByEmail email

register :: (MonadState ProfileState m, MonadAuthAction m, MonadIO m) => UserName -> Email -> PlainPass -> m ()
register userName email plainPass = do
    userNameIsAvailable <- isUserNameAvailable userName
    emailIsAvialable <- isEmailAvailable email
    userId <- addNewProfile userName email False
    setPassword userId plainPass
    return ()

authenticate :: MonadAuthAction m => UserId -> PlainPass -> m ()
authenticate userId (PlainPass plainPass) = do
    userPasswords <- gets _userPasswords
    hashedPass <- getHashedPass userId
    if validatePassword (unHashedPass hashedPass) plainPass then return () else throwError IncorrectUserNameOrPassword

tryAuthenticate :: MonadAuthAction m => UserId -> PlainPass -> m Bool
tryAuthenticate userId (PlainPass plainPass) = do
    userPasswords <- gets _userPasswords
    hashedPass <- getHashedPass userId
    return $ validatePassword (unHashedPass hashedPass) plainPass

getUserIdByEmailOrUserName :: MonadAuthAction m => Text -> m UserId
getUserIdByEmailOrUserName text = do
    lookupUserIdByEmailOrUserName text >>= maybe (throwError UserDoesNotExist) return

getHashedPass :: MonadAuthAction m =>  UserId -> m HashedPass
getHashedPass userId = do
    userPasswords <- gets _userPasswords
    getHashedPass' userPasswords userId
    

setPassword :: (MonadIO m, MonadAuthAction m) => UserId -> PlainPass -> m ()
setPassword userId (PlainPass plainPass) = do
    oldUserPasswords <- gets _userPasswords
    newUserPasswords <- setPassword' oldUserPasswords userId (PlainPass plainPass)
    userPasswords != newUserPasswords
    return ()

logOut :: MonadAuthAction m => Text -> m ()
logOut text = do
    userId <- getUserIdByEmailOrUserName text
    deleteAuthToken userId

logIn :: (MonadAuthAction m, MonadIO m) => Text -> PlainPass -> m AuthToken
logIn emailOrName password = do
    userId <- getUserIdByEmailOrUserName emailOrName
    authenticate userId password
    newToken <- generateAuthToken
    setAuthToken userId (Just newToken)
    return newToken

updatePassword :: (MonadAuthAction m, MonadIO m) => Text -> PlainPass -> PlainPass -> m ()
updatePassword userEmailOrName (PlainPass oldPass) (PlainPass newPass) = do
    userId <- getUserIdByEmailOrUserName userEmailOrName
    authenticate userId (PlainPass oldPass)
    setPassword userId (PlainPass newPass)
    return ()

-- Will be called by the Framework to set up Location, Game, and Profile actions.
getUserProfile :: MonadAuthAction m => AuthToken -> m Profile
getUserProfile authToken = do
    userTokens <- gets _userTokens
    case getOne $ userTokens @= AuthToken of
        Nothing -> throwError InvalidAuthToken
        Just userToken -> do
            mProfile <- lookupProfileByUserId $ Framework.Auth.Internal.Types.UserToken._userId userToken
            maybe (throwError UserDoesNotExist) return mProfile
