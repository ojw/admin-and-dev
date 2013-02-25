{-# LANGUAGE DeriveDataTypeable, TemplateHaskell, GeneralizedNewtypeDeriving, FlexibleContexts #-}

module Framework.Auth.Internal.Types.UserPassword where

import Control.Monad
import Control.Monad.Error
import Data.Lens
import Data.Lens.Template
import Data.Data
import Data.SafeCopy
import Data.IxSet
import Data.ByteString.Char8 ( ByteString )
import Framework.Profile ( UserId )
import Crypto.BCrypt

import Framework.Auth.Internal.Types.Error

newtype PlainPass = PlainPass { unPlainPass :: ByteString } deriving (Ord, Eq, Read, Show)
newtype HashedPass = HashedPass { unHashedPass :: ByteString } deriving (Ord, Eq, Read, Show, Data, Typeable, SafeCopy)

data UserPassword = UserPassword
    { _userId   :: UserId
    , _password :: HashedPass
    } deriving (Ord, Eq, Read, Show, Data, Typeable)

$(makeLens ''UserPassword)
$(deriveSafeCopy 0 'base ''UserPassword)
$(inferIxSet "UserPasswords" ''UserPassword 'noCalcs [''UserId, ''HashedPass])

getHashedPass' :: MonadError AuthError m => UserPasswords -> UserId -> m HashedPass
getHashedPass' userPasswords userId = do
    case fmap _password $ getOne $ userPasswords @= userId of
        Nothing -> throwError UserDoesNotExist
        Just hashedPass -> return hashedPass

setPassword' :: (MonadIO m, MonadError AuthError m) => UserPasswords -> UserId -> PlainPass -> m UserPasswords
setPassword' userPasswords userId (PlainPass plainPass) = do
    mHashedPass <- liftIO $ hashPasswordUsingPolicy slowerBcryptHashingPolicy plainPass
    case mHashedPass of
        Nothing -> throwError PasswordHashFailed
        Just hashedPass -> do
            return $ updateIx userId (UserPassword userId (HashedPass hashedPass)) userPasswords
