{-# LANGUAGE FlexibleContexts, RecordWildCards, GeneralizedNewtypeDeriving #-}

module Framework.Api where

import Control.Monad.State
import Control.Monad.Reader
import Control.Monad.Writer
import Control.Monad.RWS
import Control.Monad.Error
import Control.Lens
import Data.Text
import Data.Acid
import Data.Functor

import Framework.Location
import Framework.Auth
import Framework.Profile
import Framework.Error
import Framework.View
import Framework.Acid
import Framework.Auth.Types
import Framework.Profile.Acid

data ExternalApi = ExternalApi
    { token :: Maybe AuthToken
    , api   :: FrameworkApi
    }

{-
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
-}  

runExternalApi :: ExternalApi -> Acid -> IO (Either FrameworkError (FrameworkView, Text))
runExternalApi (ExternalApi Nothing api@(FWAuthApi authApi)) acid = runFrameworkAction (runApi api) Nothing acid
runExternalApi (ExternalApi Nothing _) acid = return $ Left UserNotLoggedIn
runExternalApi (ExternalApi (Just authToken) api) acid@Acid{..} = do
    mUserId <- query authState $ GetUserIdFromToken' authToken
    case mUserId of
        Nothing -> runFrameworkAction (throwError UserNotLoggedIn) Nothing acid
        Just userId -> do
            mProfile <- liftIO $ lookupProfileByUserId'' userId profileState
            case mProfile of
                Nothing -> runFrameworkAction (throwError UserNotLoggedIn) Nothing acid
                profile -> runFrameworkAction (runApi api) profile acid

data FrameworkApi
    = FWLocApi LocationApi
    | FWAuthApi AuthApi

class (Functor m, Monad m, MonadState Acid m) => MonadFrameworkAction m

newtype FrameworkAction a = FrameworkAction { unFrameworkAction :: ReaderT (Maybe Profile, Acid) (WriterT Text (ErrorT FrameworkError IO)) a }
    deriving (Monad, Functor, MonadWriter Text, MonadError FrameworkError, MonadIO, MonadReader (Maybe Profile, Acid))

getProfile :: FrameworkAction Profile
getProfile = view _1 <$> ask >>= maybe (throwError UserNotLoggedIn) return

runFrameworkAction :: FrameworkAction a -> Maybe Profile -> Acid -> IO (Either FrameworkError (a, Text))
runFrameworkAction (FrameworkAction action) profile acid = runErrorT $ runWriterT $ (runReaderT action) (profile, acid)

-- This will probably be rewritten to work with smaller acid handles rather than entire object.
runApi :: FrameworkApi -> FrameworkAction FrameworkView
runApi (FWAuthApi authApi) = do
    acid@Acid{..} <- view _2 <$> ask
    let action = (runAuthAction $ runAuthApi authApi) profileState authState
    result <- liftIO action
    case result of
        Left e -> throwError $ FWAuthError e
        Right (v, s, w) -> do   
            tell w
            return $ FWAuthView v
runApi (FWLocApi locationApi) = do
    acid@Acid{..} <- view _2 <$> ask
    profile <- getProfile
    result <- liftIO $ (runLocationAction $ runLocationApi locationApi) profile profileState locationState
    case result of 
        Left e -> throwError $ FWLocError e
        Right (v, s, w) -> do
            tell w
            return $ FWLocView v
