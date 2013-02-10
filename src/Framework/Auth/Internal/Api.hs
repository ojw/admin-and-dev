{-# LANGUAGE FlexibleContexts, OverloadedStrings, GeneralizedNewtypeDeriving #-}

module Framework.Auth.Internal.Api where

import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Error
import Control.Monad.RWS
import Data.Text ( Text )
import Data.Lens
import Framework.Profile
import Framework.Auth.Internal.Types.UserPassword
import Framework.Auth.Internal.Types.UserToken
import Framework.Auth.Internal.Types.AuthState
import Framework.Auth.Internal.Types.Error

data AuthApi
    = Register UserName Email PlainPass
    | LogIn Text PlainPass
    | UpdatePassword Text PlainPass PlainPass
    | LogOut Text

runAuthAction :: AuthAction a -> ProfileState -> AuthSlice -> IO (Either AuthError (a, AuthSlice, Text))
runAuthAction (AuthAction action) profileState authSlice = do
    runErrorT $ (runRWST action) profileState authSlice

runAuthApi :: (MonadIO m, MonadAuthAction m) => AuthApi -> m AuthView
runAuthApi (Register userName email plainPass) = register userName email plainPass >> return (AuthViewSuccess True)
runAuthApi (LogIn text plainPass) = logIn text plainPass >> return (AuthViewSuccess True)
runAuthApi (UpdatePassword text oldPass newPass) = updatePassword text oldPass newPass >> return (AuthViewSuccess True)
runAuthApi (LogOut text) = logOut text >> return (AuthViewSuccess True)

register :: (MonadAuthAction m, MonadIO m) => UserName -> Email -> PlainPass -> m ()
register userName email plainPass = do
    userNameIsAvailable <- isUserNameAvailable userName
    emailIsAvialable <- isEmailAvailable email
    oldProfileState <- gets _profileState
    let (userId, newProfileState) = runState (addNewProfile userName email False) oldProfileState
    profileState != newProfileState
    setPassword userId plainPass
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
