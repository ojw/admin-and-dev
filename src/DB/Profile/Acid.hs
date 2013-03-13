{-# LANGUAGE DeriveDataTypeable, MultiParamTypeClasses, TemplateHaskell, FlexibleContexts, 
    FunctionalDependencies, GeneralizedNewtypeDeriving, TypeSynonymInstances, FlexibleInstances,
    UndecidableInstances, TypeFamilies #-}

module DB.Profile.Acid 

( lookupProfileByUserId''
, addNewProfile
, lookupUserIdByEmail
, lookupUserIdByUserName
, isUserNameAvailable
, isEmailAvailable
, lookupUserIdByEmailOrUserName
, lookupProfileByUserId
, lookupUserName
)

where

import Control.Monad.Reader
import Control.Monad.State
import Data.Functor
import Data.Maybe
import Data.Monoid
import Data.Data
import Data.Text            ( Text )
import Data.Acid
import Data.Acid.Advanced
import Data.IxSet
import Control.Lens
import Data.SafeCopy

import Util.HasAcidState
import Common.Profile.Types

addNewProfile' :: UserName -> Email -> Bool -> Update ProfileState UserId
addNewProfile' userName email admin = do
    userId <- fmap (view nextUserId) get
    nextUserId %= succ
    profiles %= updateIx userId (Profile userId userName email admin)
    return userId

lookupUserName' :: UserId -> Query ProfileState (Maybe UserName)
lookupUserName' userId = do
    profiles <- view profiles <$> ask
    return $ fmap (view userName) $ getOne $ profiles @= userId

lookupUserIdByEmail' :: Email -> Query ProfileState (Maybe UserId)
lookupUserIdByEmail' email = do
    profiles <- view profiles <$> ask
    return $ fmap (view userId) $ getOne $ profiles @= email

lookupUserIdByUserName' :: UserName -> Query ProfileState (Maybe UserId)
lookupUserIdByUserName' userName = do
    profiles <- view profiles <$> ask
    return $ fmap (view userId) $ getOne $ profiles @= userName

lookupProfileByUserId' :: UserId -> Query ProfileState (Maybe Profile)
lookupProfileByUserId' userId = do
    profiles <- view profiles <$> ask
    return $ getOne $ profiles @= userId    

makeAcidic ''ProfileState ['addNewProfile', 'lookupUserIdByEmail', 'lookupUserIdByUserName', 'lookupProfileByUserId', 'lookupUserName']

addNewProfile :: (HasAcidState m ProfileState, MonadIO m) => UserName -> Email -> Bool -> m UserId
addNewProfile userName email admin = Util.HasAcidState.update $ AddNewProfile' userName email admin

lookupUserIdByEmail :: (HasAcidState m ProfileState, MonadIO m) => Email -> m (Maybe UserId)
lookupUserIdByEmail email = Util.HasAcidState.query $ LookupUserIdByEmail' email

lookupUserIdByUserName :: (HasAcidState m ProfileState, MonadIO m) => UserName -> m (Maybe UserId)
lookupUserIdByUserName userName = Util.HasAcidState.query $ LookupUserIdByUserName' userName

isUserNameAvailable :: (HasAcidState m ProfileState, MonadIO m) => UserName -> m Bool
isUserNameAvailable email = (not . isJust) <$> lookupUserIdByUserName email

isEmailAvailable :: (HasAcidState m ProfileState, MonadIO m) => Email -> m Bool
isEmailAvailable email = (not . isJust) <$> lookupUserIdByEmail email

lookupUserIdByEmailOrUserName :: (HasAcidState m ProfileState, MonadIO m) => Text -> m (Maybe UserId)
lookupUserIdByEmailOrUserName text = do
    byEmail <- lookupUserIdByEmail (Email text)
    byName <- lookupUserIdByUserName text
    return $ mplus byEmail byName

lookupProfileByUserId :: (HasAcidState m ProfileState, MonadIO m) => UserId -> m (Maybe Profile)
lookupProfileByUserId userId = Util.HasAcidState.query $ LookupProfileByUserId' userId

lookupProfileByUserId'' :: MonadIO m => UserId -> AcidState ProfileState -> m (Maybe Profile)
lookupProfileByUserId'' userId profileAcid = query' profileAcid $ LookupProfileByUserId' userId

lookupUserName :: (HasAcidState m ProfileState, MonadIO m) => UserId -> m (Maybe UserName)
lookupUserName userId = Util.HasAcidState.query $ LookupUserName' userId
