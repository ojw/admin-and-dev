{-# LANGUAGE TemplateHaskell, DeriveDataTypeable, FlexibleContexts, OverloadedStrings,
    GeneralizedNewtypeDeriving, MultiParamTypeClasses, FunctionalDependencies, FlexibleInstances,
    UndecidableInstances, TypeFamilies, ScopedTypeVariables #-}

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

newtype AuthAction a = AuthAction { unAuthAction :: RWST (AcidState ProfileState, AcidState AuthState) Text AuthSlice (ErrorT AuthError IO) a}
    deriving (Functor, Monad, MonadReader (AcidState ProfileState, AcidState AuthState), MonadState AuthSlice, MonadError AuthError, MonadIO)

instance HasAcidState AuthAction ProfileState where
    getAcidState = view _1 <$> ask

instance HasAcidState AuthAction AuthState where
    getAcidState = view _2 <$> ask

instance MonadAuthAction AuthAction 

getHashedPass'' :: UserPasswords -> UserId -> Maybe HashedPass
getHashedPass'' userPasswords userId = fmap (view password) $ getOne $ userPasswords @= userId

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

generateAuthToken :: MonadIO m => m AuthToken
generateAuthToken = do
    randomPart <- liftIO $ getEntropy 64
    return $ AuthToken $ mappend "FOO" randomPart 

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
    userPasswords <- view (authState . userPasswords) <$> get
    hashedPass <- getHashedPass userId
    return $ validatePassword (unHashedPass hashedPass) plainPass

authenticate :: UserId -> PlainPass -> AuthAction ()
authenticate userId (PlainPass plainPass) = do
    userPasswords <- view (authState . userPasswords) <$> get
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

