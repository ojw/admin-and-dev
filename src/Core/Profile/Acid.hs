{-# LANGUAGE DeriveDataTypeable, FlexibleContexts, GeneralizedNewtypeDeriving
  , MultiParamTypeClasses, OverloadedStrings, ScopedTypeVariables
  , TemplateHaskell, TypeFamilies, FlexibleInstances, RecordWildCards
  , TypeOperators #-}

module Core.Profile.Acid

where

import Control.Category             ( (.) )
import Data.Functor                 ( (<$>) )
import Control.Monad                ( mzero )
import Control.Monad.Reader         ( ask, MonadReader )
import Control.Monad.State          ( get, put )
import Control.Monad.Trans          ( MonadIO, liftIO )
import Data.SafeCopy                ( SafeCopy, base, deriveSafeCopy )
import Data.Lens.Template           ( makeLens )
import Data.Acid                    ( AcidState(..), EventState(..)
                                    , EventResult(..) , Query(..)
                                    , QueryEvent(..), Update(..)
                                    , UpdateEvent(..), IsAcidic(..), makeAcidic
                                    , openLocalState)
import Data.Data                    ( Data, Typeable )
import Data.IxSet                   ( Indexable(..), IxSet(..), (@=), Proxy(..)
                                    , getOne, ixFun, updateIx, size, null
                                    , ixSet, toList )
import Data.Lens                    ( (%=), (!=), (^$), (^=), Lens, (^%=), (^!=) )
import Data.Lens.IxSet              ( ixLens )
import Data.Text                    ( Text )
import Prelude  hiding              ( null, (.) )

import Data.Time
import Data.Aeson

import Util.HasAcidState
import Core.Auth                 ( UserId )

data Profile = Profile
    { _userId       :: UserId
    , _userName     :: UserName
    , _email        :: Email
    , _joinDate     :: UTCTime
    , _timeStamp    :: UTCTime
    } deriving (Eq, Ord, Data, Typeable, Read, Show)

newtype UserName = UserName { _unUserName :: Text } deriving (Eq, Ord, Data, Typeable, Read, Show, SafeCopy)
newtype Email = Email { _unEmail :: Text } deriving (Eq, Ord, Data, Typeable, Read, Show, SafeCopy)

$(makeLens ''Profile)
$(makeLens ''UserName)
$(makeLens ''Email)

$(deriveSafeCopy 0 'base ''Profile)

instance Indexable Profile where
    empty = ixSet [ ixFun $ \profile -> [ userId ^$ profile ]
                  , ixFun $ \profile -> [ userName ^$ profile ]
                  , ixFun $ \profile -> [ email ^$ profile ]
                  , ixFun $ \profile -> [ joinDate ^$ profile ]
                  ]

instance ToJSON Profile where
    toJSON p = object [ "userName"    .= (unUserName .userName ^$ p) 
                      , "email"       .= (unEmail . email ^$ p)
                      , "joinDate"    .= (joinDate ^$ p)
                      ]

-- for json object that might update various fields in profile
-- has a Maybe version of all fields that might be changed in profile
-- by a profile edit page
data MaybeProfile = MaybeProfile
    { _maybeEmail       :: Maybe Email
    } deriving (Eq, Ord, Data, Typeable, Read, Show)

$(makeLens ''MaybeProfile)
$(deriveSafeCopy 0 'base ''MaybeProfile)

instance FromJSON MaybeProfile where
    parseJSON (Object o) = 
        do  mEmail <- o .:? "email"
            return $ MaybeProfile (fmap Email mEmail)
    parseJSON _ = mzero


data ProfileState = ProfileState
    { _profiles  :: IxSet Profile
    }

$(makeLens ''ProfileState)
$(deriveSafeCopy 0 'base ''ProfileState)

newProfile :: UserId -> UserName -> Email -> UTCTime -> Profile
newProfile uid uname uemail ujoinDate = Profile uid uname uemail ujoinDate ujoinDate

addProfile :: UserId -> UserName -> Email -> UTCTime -> Update ProfileState UserId
addProfile uid uname mail now =
    do  profiles %= updateIx uid (newProfile uid uname mail now)
        return uid

getProfileById :: UserId -> ProfileState -> (Maybe Profile)
getProfileById uid profileState = getOne $ (profiles ^$ profileState) @= uid

getNameById :: UserId -> ProfileState -> (Maybe UserName)
getNameById uid profileState = fmap _userName $ getOne $ (profiles ^$ profileState) @= uid

getIdByName :: UserName -> ProfileState -> (Maybe UserId)
getIdByName uname profileState = fmap _userId $ getOne $ (profiles ^$ profileState) @= uname

updateProfile' :: Profile -> MaybeProfile -> Profile
updateProfile' profile maybeProfile =
    ( email ^!= (maybe (email ^$ profile) id (maybeEmail ^$ maybeProfile)) ) profile

updateProfile :: UserId -> MaybeProfile -> Update ProfileState (Maybe Profile)
updateProfile uid mProfile =
    do  profileState <- get
        case getProfileById uid profileState of
            Nothing ->  return Nothing
            Just p  ->  do let p' = updateProfile' p mProfile
                           profiles %= updateIx uid p'
                           return $ Just p'

updateTimeStamp :: UserId -> UTCTime -> Update ProfileState (Maybe Profile)
updateTimeStamp uid now =
    do  profileState <- get
        case getProfileById uid profileState of
            Nothing -> return Nothing
            Just p  -> let p' = (timeStamp ^!= now) p in
                       do profiles %= updateIx uid p'
                          return $ Just p'

askName :: UserId -> Query ProfileState (Maybe UserName)
askName uid = 
    do  profileState <- ask
        return $ getNameById uid profileState
        
askId :: UserName -> Query ProfileState (Maybe UserId)
askId uname =
    do  profileState <- ask
        return $ getIdByName uname profileState

askProfile :: UserId -> Query ProfileState (Maybe Profile)
askProfile uid =
    do  profileState <- ask
        return $ getProfileById uid profileState

isAlive :: UserId -> UTCTime -> NominalDiffTime -> Query ProfileState Bool
isAlive uid now aliveInterval =
    do  profileState <- ask
        case getProfileById uid profileState of
            Nothing -> return False
            Just p  -> let interval = diffUTCTime now (timeStamp ^$ p) in
                       return $ interval <= aliveInterval
                       
$(makeAcidic ''ProfileState ['updateProfile, 'updateTimeStamp, 'askName, 'askId, 'askProfile, 'isAlive])
