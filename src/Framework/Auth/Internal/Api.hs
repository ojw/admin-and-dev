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

newtype PlainPass = PlainPass { unPlainPass :: ByteString } deriving (Ord, Eq, Read, Show)

data AuthApi
    = Register UserName Email PlainPass
    | LogIn Text PlainPass
    | UpdatePassword Text PlainPass PlainPass
    | LogOut

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
runAuthApi (UpdatePassword text oldPass newPass) = updatePassword text oldPass newPass >> return (AuthViewSuccess True)

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

authenticate :: AuthAction m => UserId -> PlainPass -> m Bool
authenticate userId (PlainPass plainPass) = do
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

getAuthToken :: AuthAction m => UserId -> m (Maybe AuthToken)
getAuthToken userId = do
    userTokens <- gets _userTokens
    let mUserToken = getOne $ userTokens @= userId
    case mUserToken of
        Nothing -> return Nothing
        Just userToken -> return $ _authToken userToken

-- prooooooobably could make this more random
generateAuthToken :: MonadIO m => m AuthToken
generateAuthToken = return $ AuthToken "FOO"

setAuthToken :: AuthAction m => UserId -> m ()
setAuthToken userId = do
    userTokens %= updateIx userId (UserToken userId Nothing)
    return ()

updatePassword :: (AuthAction m, MonadIO m) => Text -> PlainPass -> PlainPass -> m ()
updatePassword userEmailOrName (PlainPass oldPass) (PlainPass newPass) = do
    userId <- getUserIdByEmailOrUserName userEmailOrName
    isAuthenticated <- authenticate userId (PlainPass oldPass)
    if not isAuthenticated then throwError IncorrectUserNameOrPassword else do
        setPassword userId (PlainPass newPass)
        return ()
