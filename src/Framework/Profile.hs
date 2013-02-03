{-# LANGUAGE DeriveDataTypeable, TemplateHaskell, FlexibleContexts, GeneralizedNewtypeDeriving  #-}

module Framework.Profile where

import Control.Monad.Reader
import Data.Data
import Data.Text            ( Text )
import Data.IxSet
import Data.Lens
import Data.Lens.Template
import Data.SafeCopy

newtype UserId = UserId Int deriving (Ord, Eq, Read, Show, Data, Typeable, SafeCopy)

type UserName = Text

data Profile = Profile
    { _userId   :: UserId
    , _userName :: UserName
    , _isAdmin  :: Bool
    } deriving (Ord, Eq, Read, Show, Data, Typeable)

$(makeLens ''Profile)
$(deriveSafeCopy 0 'base ''Profile)
$(inferIxSet "Profiles" ''Profile 'noCalcs [''UserId, ''UserName, ''Bool])

data ProfileState = ProfileState
    { _nextUserId    :: UserId
    , _profiles      :: Profiles
    } deriving (Ord, Eq, Read, Show, Data, Typeable)

$(makeLens ''ProfileState)
$(deriveSafeCopy 0 'base ''ProfileState)

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
