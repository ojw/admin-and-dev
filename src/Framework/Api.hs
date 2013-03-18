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
import Data.ByteString.Lazy

import Framework.Location
import Framework.Auth
import Framework.Profile
import Framework.Error
import Framework.View
import DB.Acid
import Common.Auth.Types
import DB.Profile.Acid

data FrameworkApi = FrameworkApi
    { token :: Maybe AuthToken
    , api   :: InternalApi
    }

data Frontend = Frontend
    { apiDecoder  :: ByteString -> Maybe FrameworkApi 
    , viewEncoder :: FrameworkView -> ByteString
    }

type ApiCallOutcome = Either FrameworkError (FrameworkView, Text)
type ApiRunner = FrameworkApi -> IO ApiCallOutcome
type LoggingPolicy = ApiCallOutcome -> IO FrameworkView
type FrameworkServer = FrameworkApi -> IO FrameworkView
type ApiServer = ByteString -> IO ByteString

composeServer :: FrameworkServer -> Frontend -> ApiServer
composeServer frameworkServer frontend = \byteString -> 
    case apiDecoder frontend $ byteString of
        Nothing -> return $ viewEncoder frontend $ FWError InvalidApiCall
        Just api -> (viewEncoder frontend) <$> frameworkServer api

stupidRun :: Acid -> FrameworkServer
stupidRun acid externalApi = runFrameworkApi acid externalApi >>= stupidLoggingPolicy

stupidLoggingPolicy :: LoggingPolicy
stupidLoggingPolicy (Left frameworkError) = return $ FWError frameworkError
stupidLoggingPolicy (Right (frameworkView, _)) = return frameworkView

runFrameworkApi :: Acid -> ApiRunner
runFrameworkApi acid (FrameworkApi Nothing             api@(FWAuthApi _)) = runFrameworkAction (interpretApi api) Nothing acid
runFrameworkApi acid (FrameworkApi Nothing             _)  = return $ Left UserNotLoggedIn
runFrameworkApi acid (FrameworkApi (Just authToken)    api) = do
    mUserId <- query (authState acid) $ GetUserIdFromToken' authToken
    case mUserId of
        Nothing -> runFrameworkAction (throwError UserNotLoggedIn) Nothing acid
        Just userId -> do
            mProfile <- liftIO $ lookupProfileByUserId'' userId (profileState acid)
            case mProfile of
                Nothing -> runFrameworkAction (throwError UserNotLoggedIn) Nothing acid
                profile -> runFrameworkAction (interpretApi api) profile acid

data InternalApi
    = FWLocApi LocationApi
    | FWAuthApi AuthApi

class (Functor m, Monad m, MonadState Acid m) => MonadFrameworkAction m

newtype FrameworkAction a = FrameworkAction { unFrameworkAction :: ReaderT (Maybe Profile, Acid) (WriterT Text (ErrorT FrameworkError IO)) a }
    deriving (Monad, Functor, MonadWriter Text, MonadError FrameworkError, MonadIO, MonadReader (Maybe Profile, Acid))

getProfile :: FrameworkAction Profile
getProfile = view _1 <$> ask >>= maybe (throwError UserNotLoggedIn) return

runFrameworkAction :: FrameworkAction a -> Maybe Profile -> Acid -> IO (Either FrameworkError (a, Text))
runFrameworkAction (FrameworkAction action) profile acid = runErrorT $ runWriterT $ (runReaderT action) (profile, acid)

interpretApi :: InternalApi -> FrameworkAction FrameworkView
interpretApi (FWAuthApi authApi) = do
    acid@Acid{..} <- view _2 <$> ask
    let action = (runAuthAction $ runAuthApi authApi) profileState authState
    result <- liftIO action
    case result of
        Left e -> throwError $ FWAuthError e
        Right (v, s, w) -> do   
            tell w
            return $ FWAuthView v
interpretApi (FWLocApi locationApi) = do
    acid@Acid{..} <- view _2 <$> ask
    profile <- getProfile
    result <- liftIO $ (runLocationAction $ runLocationApi locationApi) profile profileState locationState
    case result of 
        Left e -> throwError $ FWLocError e
        Right (v, s, w) -> do
            tell w
            return $ FWLocView v
