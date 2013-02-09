{-# LANGUAGE FlexibleContexts, OverloadedStrings #-}

module Framework.Auth.Internal.Api where

import Data.Functor ( (<$>) )
import Data.Maybe ( isJust, fromJust )
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Error
import Data.Text ( Text )
import Data.Lens
import Data.IxSet
import Data.ByteString.Char8 ( ByteString )
import Framework.Profile
import Framework.Auth.Internal.Types.UserPassword
import Framework.Auth.Internal.Types.UserToken
import Framework.Auth.Internal.Types.AuthState
import Crypto.BCrypt
import System.Entropy
import Data.Monoid


data AuthApi
    = Register UserName Email PlainPass
    | LogIn Text PlainPass
    | UpdatePassword Text PlainPass PlainPass
    | LogOut Text

data AuthError
    = UserNameNotAvailable
    | EmailNotAvailable
    | IncorrectUserNameOrPassword
    | UserDoesNotExist
    | PasswordHashFailed

data AuthView
    = AuthTokenView AuthToken
    | AuthViewSuccess Bool

class (Functor m, Monad m, MonadReader ProfileState m, MonadState AuthState m, MonadError AuthError m) => AuthAction m

runAuthApi :: (MonadIO m, AuthAction m, MonadState ProfileState m) => AuthApi -> m AuthView
runAuthApi (Register userName email plainPass) = register userName email plainPass >> return (AuthViewSuccess True)
runAuthApi (LogIn text plainPass) = logIn text plainPass >> return (AuthViewSuccess True)
runAuthApi (UpdatePassword text oldPass newPass) = updatePassword text oldPass newPass >> return (AuthViewSuccess True)
runAuthApi (LogOut text) = logOut text >> return (AuthViewSuccess True)

isUserNameAvailable :: (Functor m, MonadReader ProfileState m) => UserName -> m Bool
isUserNameAvailable email = (not . isJust) <$> lookupUserIdByUserName email

isEmailAvailable :: (Functor m, MonadReader ProfileState m) => Email -> m Bool
isEmailAvailable email = (not . isJust) <$> lookupUserIdByEmail email

register :: (MonadState ProfileState m, AuthAction m, MonadIO m) => UserName -> Email -> PlainPass -> m ()
register userName email plainPass = do
    userNameIsAvailable <- isUserNameAvailable userName
    emailIsAvialable <- isEmailAvailable email
    userId <- addNewProfile userName email False
    setPassword userId plainPass
    return ()

authenticate :: AuthAction m => UserId -> PlainPass -> m ()
authenticate userId (PlainPass plainPass) = do
    userPasswords <- gets _userPasswords
    hashedPass <- getHashedPass userId
    if validatePassword (unHashedPass hashedPass) plainPass then return () else throwError IncorrectUserNameOrPassword

tryAuthenticate :: AuthAction m => UserId -> PlainPass -> m Bool
tryAuthenticate userId (PlainPass plainPass) = do
    userPasswords <- gets _userPasswords
    hashedPass <- getHashedPass userId
    return $ validatePassword (unHashedPass hashedPass) plainPass

getUserIdByEmailOrUserName :: AuthAction m => Text -> m UserId
getUserIdByEmailOrUserName text = do
    lookupUserIdByEmailOrUserName text >>= maybe (throwError UserDoesNotExist) return

getHashedPass :: AuthAction m =>  UserId -> m HashedPass
getHashedPass userId = do
    userPasswords <- gets _userPasswords
    case fmap _password $ getOne $ userPasswords @= userId of
        Nothing -> throwError UserDoesNotExist
        Just hashedPass -> return hashedPass
    

setPassword :: (MonadIO m, AuthAction m) => UserId -> PlainPass -> m ()
setPassword userId (PlainPass plainPass) = do
    mHashedPass <- liftIO $ hashPasswordUsingPolicy slowerBcryptHashingPolicy plainPass
    case mHashedPass of
        Nothing -> throwError PasswordHashFailed
        Just hashedPass -> do
            userPasswords %= updateIx userId (UserPassword userId (HashedPass hashedPass))
            return ()

logOut :: AuthAction m => Text -> m ()
logOut text = do
    userId <- getUserIdByEmailOrUserName text
    deleteAuthToken userId

logIn :: (AuthAction m, MonadIO m) => Text -> PlainPass -> m AuthToken
logIn emailOrName password = do
    userId <- getUserIdByEmailOrUserName emailOrName
    authenticate userId password
    newToken <- generateAuthToken
    setAuthToken userId (Just newToken)
    return newToken

updatePassword :: (AuthAction m, MonadIO m) => Text -> PlainPass -> PlainPass -> m ()
updatePassword userEmailOrName (PlainPass oldPass) (PlainPass newPass) = do
    userId <- getUserIdByEmailOrUserName userEmailOrName
    authenticate userId (PlainPass oldPass)
    setPassword userId (PlainPass newPass)
    return ()
