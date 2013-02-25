{-# LANGUAGE FlexibleContexts, RecordWildCards, GeneralizedNewtypeDeriving #-}

module Framework.Api where

import Control.Monad.State
import Control.Monad.Reader
import Control.Monad.Writer
import Control.Monad.RWS
import Control.Monad.Error
import Data.Text

import Framework.Location
import Framework.Auth
import Framework.Profile
import Framework.Error
import Framework.View
import Framework.Acid
import Framework.Auth.Internal.Types.AuthState

data ExternalApi = ExternalApi
    { token :: Maybe AuthToken
    , api   :: FrameworkApi
    }

fancyRun :: (MonadIO m, MonadState Acid m, MonadWriter Text m) => ExternalApi -> m (Either FrameworkError FrameworkView)
fancyRun externalApi = do
    acid <- get
    result <- liftIO $ runExternalApi externalApi acid
    case result of
        Left frameworkError -> return $ Left frameworkError
        Right (frameworkView, acid, text) -> do
            put acid
            tell text
            return $ Right frameworkView

runExternalApi :: ExternalApi -> Acid -> IO (Either FrameworkError (FrameworkView, Acid, Text))
runExternalApi (ExternalApi Nothing api@(FWAuthApi authApi)) acid = runFrameworkAction (runApi api) Nothing acid
runExternalApi (ExternalApi Nothing _) acid = return $ Left UserNotLoggedIn
runExternalApi (ExternalApi (Just authToken) api) acid@Acid{..} = do
    case getUserProfile profileState authState authToken of
        Nothing -> runFrameworkAction (throwError UserNotLoggedIn) Nothing acid
        profile -> runFrameworkAction (runApi api) profile acid
    

data FrameworkApi
    = FWLocApi LocationApi
    | FWAuthApi AuthApi

class (Functor m, Monad m, MonadState Acid m) => MonadFrameworkAction m

newtype FrameworkAction a = FrameworkAction { unFrameworkAction :: RWST (Maybe Profile) Text Acid (ErrorT FrameworkError IO) a }
    deriving (Monad, Functor, MonadState Acid, MonadWriter Text, MonadError FrameworkError, MonadIO, MonadReader (Maybe Profile))

getProfile :: FrameworkAction Profile
getProfile = ask >>= maybe (throwError UserNotLoggedIn) return

runFrameworkAction :: FrameworkAction a -> Maybe Profile -> Acid -> IO (Either FrameworkError (a, Acid, Text))
runFrameworkAction (FrameworkAction action) profile acid = runErrorT $ (runRWST action) profile acid 

-- This will probably be rewritten to work with smaller acid handles rather than entire object.
runApi :: FrameworkApi -> FrameworkAction FrameworkView
runApi (FWAuthApi authApi) = do
    acid@Acid{..} <- get
    let authSlice = AuthSlice {_authState = authState, _profileState = profileState}
        action = (runAuthAction $ runAuthApi authApi) profileState authSlice
    result <- liftIO action
    case result of
        Left e -> throwError $ FWAuthError e
        Right (v, s, w) -> do   
            put $ Acid (_authState s) (_profileState s) locationState
            tell w
            return $ FWAuthView v
runApi (FWLocApi locationApi) = do
    acid@Acid{..} <- get
    profile <- getProfile
    case (runLocationAction $ runLocationApi locationApi) (profile, profileState) locationState of
        Left e -> throwError $ FWLocError e
        Right (v, s, w) -> do
            put $ Acid authState profileState s
            tell w
            return $ FWLocView v
