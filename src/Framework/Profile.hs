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

currentUserId :: (MonadReader ProfileInfo m) => m UserId
currentUserId = view (_1 . userId)

currentUserName :: (MonadReader ProfileInfo m) => m UserName
currentUserName = view (_1 . userName)

isCurrentUserAdmin :: (MonadReader ProfileInfo m) => m Bool
isCurrentUserAdmin = view (_1 . isAdmin)

lookupUserName :: (MonadReader ProfileInfo m) => UserId -> m (Maybe UserName)
lookupUserName userId = do
    profiles <- view (_2 . profiles)
    return $ fmap (view userName) $ getOne $ profiles @= userId

lookupUserIdByEmail :: (MonadReader ProfileState m) => Email -> m (Maybe UserId)
lookupUserIdByEmail email = do
    profiles <- view profiles
    return $ fmap (view userId) $ getOne $ profiles @= email

lookupUserIdByUserName :: (MonadReader ProfileState m) => UserName -> m (Maybe UserId)
lookupUserIdByUserName userName = do
    profiles <- view profiles
    return $ fmap (view userId) $ getOne $ profiles @= userName

lookupUserIdByEmailOrUserName :: (MonadReader ProfileState m) => Text -> m (Maybe UserId)
lookupUserIdByEmailOrUserName text = do
    profiles <- view profiles
    byEmail <- lookupUserIdByEmail (Email text)
    byName <- lookupUserIdByUserName text
    return $ mplus byEmail byName

lookupProfileByUserId :: (MonadReader ProfileState m) => UserId -> m (Maybe Profile)
lookupProfileByUserId userId = do
    profiles <- view profiles
    return $ getOne $ profiles @= userId    

addNewProfile :: (Functor m, MonadState ProfileState m) => UserName -> Email -> Bool -> m UserId
addNewProfile userName email admin = do
    userId <- fmap (view nextUserId) get
    nextUserId %= succ
    profiles %= updateIx userId (Profile userId userName email admin)
    return userId

isUserNameAvailable :: (Functor m, MonadReader ProfileState m) => UserName -> m Bool
isUserNameAvailable email = (not . isJust) <$> lookupUserIdByUserName email

isEmailAvailable :: (Functor m, MonadReader ProfileState m) => Email -> m Bool
isEmailAvailable email = (not . isJust) <$> lookupUserIdByEmail email
