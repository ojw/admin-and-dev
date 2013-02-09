{-# LANGUAGE TemplateHaskell, DeriveDataTypeable, FlexibleContexts, OverloadedStrings #-}

module Framework.Auth.Internal.Types.AuthState where

import Data.SafeCopy
import Data.Lens
import Data.Lens.Template
import Data.Data
import Data.IxSet
import Control.Monad.State
import System.Entropy
import Data.Monoid
import Data.ByteString.Char8 ( ByteString )

import Framework.Profile
import Framework.Auth.Internal.Types.UserPassword
import Framework.Auth.Internal.Types.UserToken

newtype PlainPass = PlainPass { unPlainPass :: ByteString } deriving (Ord, Eq, Read, Show)

data AuthState = AuthState
    { _userPasswords    :: UserPasswords
    , _userTokens       :: UserTokens
    } deriving (Ord, Eq, Read, Show, Data, Typeable)

$(makeLens ''AuthState)
$(deriveSafeCopy 0 'base ''AuthState)

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
