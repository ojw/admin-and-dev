{-# LANGUAGE TemplateHaskell, DeriveDataTypeable, FlexibleContexts, OverloadedStrings,
    GeneralizedNewtypeDeriving  #-}

module Framework.Auth.Internal.Types.AuthState where

import Control.Monad.Reader
import Control.Monad.Error
import Control.Monad.RWS
import Data.Text
import Data.SafeCopy
import Data.Lens
import Data.Lens.Template
import Data.Data
import Data.IxSet
import Control.Monad.State
import System.Entropy
import Data.Monoid
import Data.ByteString.Char8 ( ByteString )
import Crypto.BCrypt

import Framework.Profile
import Framework.Auth.Internal.Types.UserPassword
import Framework.Auth.Internal.Types.UserToken
import Framework.Auth.Internal.Types.Error


data AuthState = AuthState
    { _userPasswords    :: UserPasswords
    , _userTokens       :: UserTokens
    } deriving (Ord, Eq, Read, Show, Data, Typeable)

$(makeLens ''AuthState)
$(deriveSafeCopy 0 'base ''AuthState)

data AuthView
    = AuthTokenView AuthToken
    | AuthViewSuccess Bool

class (Functor m, Monad m, MonadReader ProfileState m, MonadState AuthState m, MonadError AuthError m) => MonadAuthAction m

newtype AuthAction a = AuthAction { unAuthAction :: RWST ProfileState Text AuthState (ErrorT AuthError IO) a}
    deriving (Functor, Monad, MonadReader ProfileState, MonadState AuthState, MonadError AuthError)

instance MonadAuthAction AuthAction 

deleteAuthToken :: MonadState AuthState m => UserId -> m ()
deleteAuthToken userId = setAuthToken userId Nothing

setAuthToken :: MonadState AuthState m => UserId -> Maybe AuthToken -> m ()
setAuthToken userId authToken = do
    userTokens %= updateIx userId (UserToken userId authToken)
    return () 

getAuthToken :: MonadState AuthState m => UserId -> m (Maybe AuthToken)
getAuthToken userId = do
    userTokens <- gets _userTokens
    let mUserToken = getOne $ userTokens @= userId
    case mUserToken of
        Nothing -> return Nothing
        Just userToken -> return $ _authToken userToken

generateAuthToken :: MonadIO m => m AuthToken
generateAuthToken = do
    randomPart <- liftIO $ getEntropy 64
    return $ AuthToken $ mappend "FOO" randomPart 

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

-- Will be called by the Framework to set up Location, Game, and Profile actions.
getUserProfile :: MonadAuthAction m => AuthToken -> m Profile
getUserProfile authToken = do
    userTokens <- gets _userTokens
    case getOne $ userTokens @= AuthToken of
        Nothing -> throwError InvalidAuthToken
        Just userToken -> do
            mProfile <- lookupProfileByUserId $ Framework.Auth.Internal.Types.UserToken._userId userToken
            maybe (throwError UserDoesNotExist) return mProfile
