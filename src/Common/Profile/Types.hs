{-# LANGUAGE DeriveDataTypeable, MultiParamTypeClasses, TemplateHaskell, FlexibleContexts, 
    FunctionalDependencies, GeneralizedNewtypeDeriving, TypeSynonymInstances, FlexibleInstances,
    UndecidableInstances, TypeFamilies #-}

module Common.Profile.Types where

import Control.Monad.Reader
import Control.Monad.State
import Data.Functor
import Data.Maybe
import Data.Monoid
import Data.Data
import Data.Text            ( Text )
import Data.Acid
import Data.IxSet
import Control.Lens
import Data.SafeCopy

import Util.HasAcidState

data ProfileError = UserNameNotAvailable | EmailNotAvailable
    
newtype UserId = UserId Int deriving (Ord, Eq, Read, Show, Data, Typeable, SafeCopy, Enum)
newtype Email = Email Text deriving (Ord, Eq, Read, Show, Data, Typeable, SafeCopy)

type UserName = Text

data Profile = Profile
    { _profileUserId   :: UserId
    , _profileUserName :: UserName
    , _profileEmail    :: Email
    , _profileIsAdmin  :: Bool
    } deriving (Ord, Eq, Read, Show, Data, Typeable)

inferIxSet "Profiles" ''Profile 'noCalcs [''UserId, ''UserName, ''Email, ''Bool]

data ProfileState = ProfileState
    { _psNextUserId    :: UserId
    , _psProfiles      :: Profiles
    } deriving (Ord, Eq, Read, Show, Data, Typeable) 

initialProfileState :: ProfileState
initialProfileState = ProfileState (UserId 1) empty

makeFields ''Profile
makeFields ''ProfileState
deriveSafeCopy 0 'base ''Profile
deriveSafeCopy 0 'base ''ProfileState

data ProfileView = ProfileView Profile

data ProfileApi = ProfileApi

class HasUserProfile m where
    getCurrentUserProfile :: m Profile

getCurrentUserId :: (Monad m, HasUserProfile m) => m UserId
getCurrentUserId = getCurrentUserProfile >>= return . (view userId)

getCurrentUserName :: (Monad m, HasUserProfile m) => m UserName
getCurrentUserName = getCurrentUserProfile >>= return . (view userName)

isCurrentUserAdmin :: (Monad m, HasUserProfile m) => m Bool
isCurrentUserAdmin = getCurrentUserProfile >>= return . (view isAdmin)
