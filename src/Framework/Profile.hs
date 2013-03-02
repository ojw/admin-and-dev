{-# LANGUAGE DeriveDataTypeable, MultiParamTypeClasses, TemplateHaskell, FlexibleContexts, 
    FunctionalDependencies, GeneralizedNewtypeDeriving, TypeSynonymInstances, FlexibleInstances  #-}

module Framework.Profile where

import Control.Monad.Reader
import Control.Monad.State
import Data.Functor
import Data.Maybe
import Data.Monoid
import Data.Data
import Data.Text            ( Text )
import Data.IxSet
import Control.Lens
import Data.SafeCopy

newtype UserId = UserId Int deriving (Ord, Eq, Read, Show, Data, Typeable, SafeCopy, Enum)
newtype Email = Email Text deriving (Ord, Eq, Read, Show, Data, Typeable, SafeCopy)

type UserName = Text

data Profile = Profile
    { _profileUserId   :: UserId
    , _profileUserName :: UserName
    , _profileEmail    :: Email
    , _profileIsAdmin  :: Bool
    } deriving (Ord, Eq, Read, Show, Data, Typeable)

makeFields ''Profile
$(deriveSafeCopy 0 'base ''Profile)
$(inferIxSet "Profiles" ''Profile 'noCalcs [''UserId, ''UserName, ''Email, ''Bool])

data ProfileState = ProfileState
    { _psNextUserId    :: UserId
    , _psProfiles      :: Profiles
    } deriving (Ord, Eq, Read, Show, Data, Typeable)

makeFields ''ProfileState
$(deriveSafeCopy 0 'base ''ProfileState)

data ProfileView = ProfileView
data ProfileApi = ProfileApi

type ProfileInfo = (Profile, ProfileState)

class HasUserProfileState m where
    getUserProfileState :: m ProfileState

getNextUserId :: (Monad m, HasUserProfileState m) => m UserId
getNextUserId = getUserProfileState >>= return . (view nextUserId)

getUserProfiles :: (Monad m, HasUserProfileState m) => m Profiles
getUserProfiles = getUserProfileState >>= return . (view profiles)

class HasUserProfile m where
    getCurrentUserProfile :: m Profile

getCurrentUserId :: (Monad m, HasUserProfile m) => m UserId
getCurrentUserId = getCurrentUserProfile >>= return . (view userId)

getCurrentUserName :: (Monad m, HasUserProfile m) => m UserName
getCurrentUserName = getCurrentUserProfile >>= return . (view userName)

isCurrentUserAdmin :: (Monad m, HasUserProfile m) => m Bool
isCurrentUserAdmin = getCurrentUserProfile >>= return . (view isAdmin)

lookupUserName :: (Monad m, HasUserProfileState m) => UserId -> m (Maybe UserName)
lookupUserName userId = do
    profiles <- getUserProfiles
    return $ fmap (view userName) $ getOne $ profiles @= userId

lookupUserIdByEmail :: (Monad m, HasUserProfileState m) => Email -> m (Maybe UserId)
lookupUserIdByEmail email = do
    profiles <- getUserProfiles
    return $ fmap (view userId) $ getOne $ profiles @= email

lookupUserIdByUserName :: (Monad m, HasUserProfileState m) => UserName -> m (Maybe UserId)
lookupUserIdByUserName userName = do
    profiles <- getUserProfiles
    return $ fmap (view userId) $ getOne $ profiles @= userName

lookupUserIdByEmailOrUserName :: (Monad m, HasUserProfileState m) => Text -> m (Maybe UserId)
lookupUserIdByEmailOrUserName text = do
    profiles <- getUserProfiles
    byEmail <- lookupUserIdByEmail (Email text)
    byName <- lookupUserIdByUserName text
    return $ mplus byEmail byName

lookupProfileByUserId :: ProfileState -> UserId -> Maybe Profile
lookupProfileByUserId profileState userId = getOne $ view profiles profileState @= userId    

addNewProfile :: (Functor m, MonadState ProfileState m) => UserName -> Email -> Bool -> m UserId
addNewProfile userName email admin = do
    userId <- fmap (view nextUserId) get
    nextUserId %= succ
    profiles %= updateIx userId (Profile userId userName email admin)
    return userId

isUserNameAvailable :: (Functor m, Monad m, HasUserProfileState m) => UserName -> m Bool
isUserNameAvailable email = (not . isJust) <$> lookupUserIdByUserName email

isEmailAvailable :: (Functor m, Monad m, HasUserProfileState m) => Email -> m Bool
isEmailAvailable email = (not . isJust) <$> lookupUserIdByEmail email
