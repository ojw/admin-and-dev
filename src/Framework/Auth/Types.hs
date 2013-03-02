{-# LANGUAGE TemplateHaskell, DeriveDataTypeable, FlexibleContexts, OverloadedStrings,
    GeneralizedNewtypeDeriving, MultiParamTypeClasses, FunctionalDependencies, FlexibleInstances,
    UndecidableInstances  #-}

module Framework.Auth.Types where

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

data AuthError
    = IncorrectUserNameOrPassword
    | UserDoesNotExist
    | PasswordHashFailed
    | DefaultAuthError
    | InvalidAuthToken
    | AuthProfileError ProfileError

instance Error AuthError where
    noMsg = DefaultAuthError

newtype PlainPass = PlainPass { unPlainPass :: ByteString } deriving (Ord, Eq, Read, Show)
newtype HashedPass = HashedPass { unHashedPass :: ByteString } deriving (Ord, Eq, Read, Show, Data, Typeable, SafeCopy)

data UserPassword = UserPassword
    { _upwUserId   :: UserId
    , _upwPassword :: HashedPass
    } deriving (Ord, Eq, Read, Show, Data, Typeable)

inferIxSet "UserPasswords" ''UserPassword 'noCalcs [''UserId, ''HashedPass]

newtype AuthToken = AuthToken ByteString deriving (Ord, Eq, Read, Show, Data, Typeable, SafeCopy)

data UserToken = UserToken
    { _utUserId       :: UserId
    , _utAuthToken    :: Maybe AuthToken
    } deriving (Ord, Eq, Read, Show, Data, Typeable)

inferIxSet "UserTokens" ''UserToken 'noCalcs [''UserId, ''AuthToken]

data AuthState = AuthState
    { _asUserPasswords    :: UserPasswords
    , _asUserTokens       :: UserTokens
    } deriving (Ord, Eq, Read, Show, Data, Typeable)

data AuthView
    = AuthTokenView AuthToken
    | AuthViewSuccess Bool

data AuthSlice = AuthSlice
    { _asAuthState :: AuthState
    , _asProfileState :: ProfileState
    }

makeFields ''UserPassword
makeFields ''UserToken
makeFields ''AuthState
makeFields ''AuthSlice

deriveSafeCopy 0 'base ''UserPassword
deriveSafeCopy 0 'base ''UserToken
deriveSafeCopy 0 'base ''AuthState

class (Functor m, Monad m, HasAcidState m ProfileState, MonadState AuthSlice m, MonadError AuthError m) => MonadAuthAction m

newtype AuthAction a = AuthAction { unAuthAction :: RWST (AcidState ProfileState) Text AuthSlice (ErrorT AuthError IO) a}
    deriving (Functor, Monad, MonadReader (AcidState ProfileState), MonadState AuthSlice, MonadError AuthError, MonadIO)

instance HasAcidState AuthAction ProfileState where
    getAcidState = ask

instance MonadAuthAction AuthAction 

getHashedPass' :: MonadError AuthError m => UserPasswords -> UserId -> m HashedPass
getHashedPass' userPasswords userId = do
    case fmap (view password) $ getOne $ userPasswords @= userId of
        Nothing -> throwError UserDoesNotExist
        Just hashedPass -> return hashedPass

setPassword' :: (MonadIO m, MonadError AuthError m) => UserPasswords -> UserId -> PlainPass -> m UserPasswords
setPassword' userPasswords userId (PlainPass plainPass) = do
    mHashedPass <- liftIO $ hashPasswordUsingPolicy slowerBcryptHashingPolicy plainPass
    case mHashedPass of
        Nothing -> throwError PasswordHashFailed
        Just hashedPass -> do
            return $ updateIx userId (UserPassword userId (HashedPass hashedPass)) userPasswords

deleteAuthToken :: MonadAuthAction m => UserId -> m ()
deleteAuthToken userId = setAuthToken userId Nothing

setAuthToken :: MonadAuthAction m => UserId -> Maybe AuthToken -> m ()
setAuthToken userId authToken = do
    authState %= (userTokens %~ updateIx userId (UserToken userId authToken))
    return () 

getAuthToken :: MonadAuthAction m => UserId -> m (Maybe AuthToken)
getAuthToken userId = do
    userTokens <- view (authState . userTokens) <$> get
    let mUserToken = getOne $ userTokens @= userId
    case mUserToken of
        Nothing -> return Nothing
        Just userToken -> return $ view authToken userToken

generateAuthToken :: MonadIO m => m AuthToken
generateAuthToken = do
    randomPart <- liftIO $ getEntropy 64
    return $ AuthToken $ mappend "FOO" randomPart 

authenticate :: MonadAuthAction m => UserId -> PlainPass -> m ()
authenticate userId (PlainPass plainPass) = do
    userPasswords <- view (authState . userPasswords) <$> get
    hashedPass <- getHashedPass userId
    if validatePassword (unHashedPass hashedPass) plainPass then return () else throwError IncorrectUserNameOrPassword

tryAuthenticate :: MonadAuthAction m => UserId -> PlainPass -> m Bool
tryAuthenticate userId (PlainPass plainPass) = do
    userPasswords <- view (authState . userPasswords) <$> get
    hashedPass <- getHashedPass userId
    return $ validatePassword (unHashedPass hashedPass) plainPass

getUserIdByEmailOrUserName :: (MonadIO m, MonadAuthAction m) => Text -> m UserId
getUserIdByEmailOrUserName text = do
    lookupUserIdByEmailOrUserName text >>= maybe (throwError UserDoesNotExist) return

getHashedPass :: MonadAuthAction m =>  UserId -> m HashedPass
getHashedPass userId = do
    userPasswords <- view (authState . userPasswords) <$> get
    getHashedPass' userPasswords userId
    

setPassword :: (MonadIO m, MonadAuthAction m) => UserId -> PlainPass -> m ()
setPassword userId (PlainPass plainPass) = do
    oldUserPasswords <- view (authState . userPasswords) <$> get
    newUserPasswords <- setPassword' oldUserPasswords userId (PlainPass plainPass)
    authState %= (userPasswords .~ newUserPasswords)
    return ()

-- Will be called by the Framework to set up Location, Game, and Profile actions.
{-
getUserProfile :: MonadAuthAction m => AuthToken -> m Profile
getUserProfile authToken = do
    userTokens <- _userTokens <$> gets _authState
    case getOne $ userTokens @= authToken of
        Nothing -> throwError InvalidAuthToken
        Just userToken -> do
            mProfile <- lookupProfileByUserId $ Framework.Auth.Types.UserToken._userId userToken
            maybe (throwError UserDoesNotExist) return mProfile
-}

getUserProfile :: ProfileState -> AuthState -> AuthToken -> Maybe Profile
getUserProfile profileState authState authToken = do
    case getOne $ view userTokens authState @= authToken of
        Nothing -> Nothing
        Just userToken -> lookupProfileByUserId profileState (view userId userToken)
