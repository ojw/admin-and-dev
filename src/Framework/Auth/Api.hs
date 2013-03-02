{-# LANGUAGE FlexibleContexts, OverloadedStrings, GeneralizedNewtypeDeriving #-}

module Framework.Auth.Api where

import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Error
import Control.Monad.RWS
import Data.Text ( Text )
import Control.Lens
import Data.Functor
import Framework.Profile
import Framework.Auth.Types

data AuthApi
    = Register UserName Email PlainPass
    | LogIn Text PlainPass
    | UpdatePassword Text PlainPass PlainPass
    | LogOut Text

runAuthAction :: AuthAction a -> ProfileState -> AuthSlice -> IO (Either AuthError (a, AuthSlice, Text))
runAuthAction (AuthAction action) profileState authSlice = do
    runErrorT $ (runRWST action) profileState authSlice

runAuthApi :: AuthApi -> AuthAction AuthView
runAuthApi (Register userName email plainPass) = register userName email plainPass >> return (AuthViewSuccess True)
runAuthApi (LogIn text plainPass) = logIn text plainPass >> return (AuthViewSuccess True)
runAuthApi (UpdatePassword text oldPass newPass) = updatePassword text oldPass newPass >> return (AuthViewSuccess True)
runAuthApi (LogOut text) = logOut text >> return (AuthViewSuccess True)

register :: UserName -> Email -> PlainPass -> AuthAction ()
register userName email plainPass = do
    userNameIsAvailable <- isUserNameAvailable userName
    emailIsAvialable <- isEmailAvailable email
    oldProfileState <- view profileState <$> get
    let (userId, newProfileState) = runState (addNewProfile userName email False) oldProfileState
    profileState .= newProfileState
    setPassword userId plainPass
    return ()

logOut :: Text -> AuthAction ()
logOut text = do
    userId <- getUserIdByEmailOrUserName text
    deleteAuthToken userId

logIn :: Text -> PlainPass -> AuthAction AuthToken
logIn emailOrName password = do
    userId <- getUserIdByEmailOrUserName emailOrName
    authenticate userId password
    newToken <- generateAuthToken
    setAuthToken userId (Just newToken)
    return newToken

updatePassword :: Text -> PlainPass -> PlainPass -> AuthAction ()
updatePassword userEmailOrName (PlainPass oldPass) (PlainPass newPass) = do
    userId <- getUserIdByEmailOrUserName userEmailOrName
    authenticate userId (PlainPass oldPass)
    setPassword userId (PlainPass newPass)
    return ()
