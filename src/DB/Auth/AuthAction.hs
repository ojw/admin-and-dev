{-# LANGUAGE TemplateHaskell, DeriveDataTypeable, FlexibleContexts, OverloadedStrings,
    GeneralizedNewtypeDeriving, MultiParamTypeClasses, FunctionalDependencies, FlexibleInstances,
    UndecidableInstances, TypeFamilies, ScopedTypeVariables #-}

module DB.Auth.AuthAction where

import Control.Monad.Reader
import Control.Monad.Error
import Control.Monad.RWS
import Data.Functor
import Data.Text
import Data.SafeCopy
import Control.Lens
import Data.Data
import Data.Acid
import Data.IxSet
import Control.Monad.State
import System.Entropy
import Data.Monoid
import Data.ByteString.Char8 ( ByteString )
import Crypto.BCrypt

import Util.HasAcidState
import Framework.Profile
import Common.Auth.Types

instance Error AuthError where
    noMsg = DefaultAuthError

deriveSafeCopy 0 'base ''UserPassword
deriveSafeCopy 0 'base ''UserToken
deriveSafeCopy 0 'base ''AuthState

class (Functor m, Monad m, HasAcidState m ProfileState, HasAcidState m AuthState, MonadError AuthError m) => MonadAuthAction m

newtype AuthAction a = AuthAction { unAuthAction :: RWST (AcidState ProfileState, AcidState AuthState) Text () (ErrorT AuthError IO) a}
    deriving (Functor, Monad, MonadReader (AcidState ProfileState, AcidState AuthState), MonadError AuthError, MonadIO)

instance HasAcidState AuthAction ProfileState where
    getAcidState = view _1 <$> ask

instance HasAcidState AuthAction AuthState where
    getAcidState = view _2 <$> ask

instance MonadAuthAction AuthAction 

getHashedPass' :: UserId -> Query AuthState (Maybe HashedPass)
getHashedPass' userId = do
    userPasswords <- fmap (view userPasswords) ask
    return $ getHashedPass'' userPasswords userId

setPassword' :: UserId -> HashedPass -> Update AuthState ()
setPassword' userId hashedPass = userPasswords %= updateIx userId (UserPassword userId hashedPass)

setAuthToken' :: UserId -> Maybe AuthToken -> Update AuthState ()
setAuthToken' userId authToken = do
    userTokens %= updateIx userId (UserToken userId authToken)
    return () 

getAuthToken' :: UserId -> Query AuthState (Maybe AuthToken)
getAuthToken' userId = do
    userTokens <- view  userTokens <$> ask
    let mUserToken = getOne $ userTokens @= userId
    case mUserToken of
        Nothing -> return Nothing
        Just userToken -> return $ view authToken userToken

getUserIdByEmailOrUserName :: (MonadIO m, MonadAuthAction m) => Text -> m UserId
getUserIdByEmailOrUserName text = do
    lookupUserIdByEmailOrUserName text >>= maybe (throwError UserDoesNotExist) return

getUserIdFromToken' :: AuthToken -> Query AuthState (Maybe UserId)
getUserIdFromToken' authToken = do
    authState <- ask
    return $ fmap (view userId) $ getOne $ view userTokens authState @= authToken

makeAcidic ''AuthState ['getHashedPass', 'setPassword', 'setAuthToken', 'getAuthToken', 'getUserIdFromToken']

getUserIdFromToken :: (HasAcidState m AuthState, MonadIO m) => AuthToken -> m (Maybe UserId)
getUserIdFromToken authToken = Util.HasAcidState.query $ GetUserIdFromToken' authToken

getAuthToken :: UserId -> AuthAction (Maybe AuthToken)
getAuthToken userId = Util.HasAcidState.query $ GetAuthToken' userId

setPassword :: UserId -> PlainPass -> AuthAction ()
setPassword userId (PlainPass plainPass) = do
    mHashedPass <- liftIO $ hashPasswordUsingPolicy slowerBcryptHashingPolicy plainPass
    case mHashedPass of
        Nothing -> throwError PasswordHashFailed
        Just hashedPass -> do
            Util.HasAcidState.update $ SetPassword' userId (HashedPass hashedPass)

getHashedPass :: UserId -> AuthAction HashedPass
getHashedPass userId = do
    mHashedPass <- Util.HasAcidState.query $ GetHashedPass' userId
    maybe (throwError UserDoesNotExist) return mHashedPass 

tryAuthenticate :: UserId -> PlainPass -> AuthAction Bool
tryAuthenticate userId (PlainPass plainPass) = do
    hashedPass <- getHashedPass userId
    return $ validatePassword (unHashedPass hashedPass) plainPass

authenticate :: UserId -> PlainPass -> AuthAction ()
authenticate userId (PlainPass plainPass) = do
    hashedPass <- getHashedPass userId
    if validatePassword (unHashedPass hashedPass) plainPass then return () else throwError IncorrectUserNameOrPassword

deleteAuthToken :: UserId -> AuthAction ()
deleteAuthToken userId = Util.HasAcidState.update $ SetAuthToken' userId Nothing

setAuthToken :: UserId -> Maybe AuthToken -> AuthAction ()
setAuthToken userId authToken = Util.HasAcidState.update $ SetAuthToken' userId authToken

getUserProfile :: (HasAcidState m ProfileState, HasAcidState m AuthState, MonadIO m) => AuthToken -> m (Maybe Profile)
getUserProfile authToken = do
    mUserId <- getUserIdFromToken authToken
    case mUserId of
        Nothing -> return Nothing
        Just userId -> lookupProfileByUserId userId 
