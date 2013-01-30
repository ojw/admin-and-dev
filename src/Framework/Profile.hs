{-# LANGUAGE DeriveDataTypeable, TemplateHaskell, FlexibleContexts  #-}

module Framework.Profile where

import Control.Monad.Reader
import Data.Data
import Data.Text            ( Text )
import Framework.Auth       ( UserId )
import Data.IxSet
import Data.Lens
import Data.Lens.Template
import Data.SafeCopy

type UserName = Text

data Profile = Profile
    { _userId   :: UserId
    , _userName :: UserName
    , _isAdmin  :: Bool
    } deriving (Ord, Eq, Read, Show, Data, Typeable)

$(makeLens ''Profile)
$(deriveSafeCopy 0 'base ''Profile)
$(inferIxSet "Profiles" ''Profile 'noCalcs [''UserId, ''UserName, ''Bool])

currentUserId :: (MonadReader Profile m) => m UserId
currentUserId = asks _userId

currentUserName :: (MonadReader Profile m) => m UserName
currentUserName = asks _userName

isCurrentUserAdmin :: (MonadReader Profile m) => m Bool
isCurrentUserAdmin = asks _isAdmin

data ProfileState = ProfileState
    { _nextUserId    :: UserId
    , _profiles      :: Profiles
    } deriving (Ord, Eq, Read, Show, Data, Typeable)

$(makeLens ''ProfileState)
$(deriveSafeCopy 0 'base ''ProfileState)

lookupUserName :: (MonadReader ProfileState m) => UserId -> m (Maybe UserName)
lookupUserName userId = do
    profiles <- asks _profiles
    return $ fmap _userName $ getOne $ profiles @= userId
