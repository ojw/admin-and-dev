{-# LANGUAGE DeriveDataTypeable, TemplateHaskell, FlexibleContexts, GeneralizedNewtypeDeriving  #-}

module Framework.Profile where

import Control.Monad.Reader
import Control.Monad.State
import Data.Monoid
import Data.Data
import Data.Text            ( Text )
import Data.IxSet
import Data.Lens
import Data.Lens.Template
import Data.SafeCopy

newtype UserId = UserId Int deriving (Ord, Eq, Read, Show, Data, Typeable, SafeCopy, Enum)
newtype Email = Email Text deriving (Ord, Eq, Read, Show, Data, Typeable, SafeCopy)

type UserName = Text

data Profile = Profile
    { _userId   :: UserId
    , _userName :: UserName
    , _email    :: Email
    , _isAdmin  :: Bool
    } deriving (Ord, Eq, Read, Show, Data, Typeable)

$(makeLens ''Profile)
$(deriveSafeCopy 0 'base ''Profile)
$(inferIxSet "Profiles" ''Profile 'noCalcs [''UserId, ''UserName, ''Email, ''Bool])

data ProfileState = ProfileState
    { _nextUserId    :: UserId
    , _profiles      :: Profiles
    } deriving (Ord, Eq, Read, Show, Data, Typeable)

$(makeLens ''ProfileState)
$(deriveSafeCopy 0 'base ''ProfileState)

data ProfileView = ProfileView
data ProfileApi = ProfileApi

type ProfileInfo = (Profile, ProfileState)

currentUserId :: (MonadReader ProfileInfo m) => m UserId
currentUserId = asks (_userId . fst)

currentUserName :: (MonadReader ProfileInfo m) => m UserName
currentUserName = asks (_userName . fst)

isCurrentUserAdmin :: (MonadReader ProfileInfo m) => m Bool
isCurrentUserAdmin = asks (_isAdmin . fst)

lookupUserName :: (MonadReader ProfileInfo m) => UserId -> m (Maybe UserName)
lookupUserName userId = do
    profiles <- asks (_profiles . snd)
    return $ fmap _userName $ getOne $ profiles @= userId

lookupUserIdByEmail :: (MonadReader ProfileState m) => Email -> m (Maybe UserId)
lookupUserIdByEmail email = do
    profiles <- asks _profiles
    return $ fmap _userId $ getOne $ profiles @= email

lookupUserIdByUserName :: (MonadReader ProfileState m) => UserName -> m (Maybe UserId)
lookupUserIdByUserName userName = do
    profiles <- asks _profiles
    return $ fmap _userId $ getOne $ profiles @= userName

lookupUserIdByEmailOrUserName :: (MonadReader ProfileState m) => Text -> m (Maybe UserId)
lookupUserIdByEmailOrUserName text = do
    profiles <- asks _profiles
    byEmail <- lookupUserIdByEmail (Email text)
    byName <- lookupUserIdByUserName text
    return $ mplus byEmail byName

lookupProfileByUserId :: (MonadReader ProfileState m) => UserId -> m (Maybe Profile)
lookupProfileByUserId userId = do
    profiles <- asks _profiles
    return $ getOne $ profiles @= userId    

addNewProfile :: (MonadState ProfileState m) => UserName -> Email -> Bool -> m UserId
addNewProfile userName email admin = do
    userId <- gets _nextUserId
    nextUserId %= succ
    profiles %= updateIx userId (Profile userId userName email admin)
    return userId
