{-# LANGUAGE FlexibleContexts, RecordWildCards, GeneralizedNewtypeDeriving #-}

module Framework where

import Control.Monad.State
import Control.Monad.Reader
import Control.Monad.Writer
import Control.Monad.RWS
import Control.Monad.Error
import Data.Text

import Framework.Location
import Framework.Location.Internal.Views.LocationView
import Framework.Location.Internal.Types.Location
import Framework.Auth
import Framework.Auth.Internal.Types.AuthState
import Framework.Profile

data Acid = Acid
    { authState :: AuthState
    , profileState :: ProfileState
    , locationState :: LocationState
    }

data FrameworkError
    = FWLocError LocationError
    | FWAuthError AuthError
    | DefaultError

instance Error FrameworkError where
    noMsg = DefaultError

data FrameworkApi
    = FWLocApi LocationApi
    | FWAuthApi AuthApi
    | FWProfileApi ProfileApi

data FrameworkView
    = FrameworkView 
    | FWLocView LocationView
    | FWAuthView AuthView
    | FWProfileView ProfileView

class (Functor m, Monad m, MonadState Acid m) => MonadFrameworkAction m

newtype FrameworkAction a = FrameworkAction { unFrameworkAction :: RWST (Maybe Profile) Text Acid (ErrorT FrameworkError IO) a }
    deriving (Monad, Functor, MonadState Acid, MonadWriter Text, MonadError FrameworkError, MonadIO, MonadReader (Maybe Profile))

runFrameworkAction :: FrameworkAction a -> Maybe Profile -> Acid -> IO (Either FrameworkError (a, Acid, Text))
runFrameworkAction (FrameworkAction action) profile acid = runErrorT $ (runRWST action) profile acid

runApi :: FrameworkApi -> FrameworkAction FrameworkView
runApi (FWAuthApi authApi) = return FrameworkView
runApi (FWProfileApi profileApi) = return FrameworkView
runApi (FWLocApi locationApi) = do
    acid@Acid{..} <- get
    mProfile <- ask
    case mProfile of
        Nothing -> throwError DefaultError
        Just profile -> do
            let locationAction = runLocationApi locationApi
                locationValue = (runLocationAction locationAction) (profile, profileState) locationState
            case locationValue of
                Left e -> throwError $ FWLocError e
                Right (v, s, w) -> do
                    put $ Acid authState profileState s
                    tell w
                    return $ FWLocView v
