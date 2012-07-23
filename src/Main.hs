{-# LANGUAGE DeriveDataTypeable, TemplateHaskell, TypeFamilies
           , GeneralizedNewtypeDeriving, OverloadedStrings #-}

module Main where

import Data.Text (Text)
import qualified Data.Text as Text
import Data.SafeCopy        (SafeCopy, base, deriveSafeCopy)
import Data.Data            (Data, Typeable)
import Data.Lens            ((%=), (!=), (^$))
import Data.Lens.Template   (makeLens)
import Data.Acid            ( AcidState(..), EventState(..), EventResult(..)
                            , Query(..), QueryEvent(..), Update(..), UpdateEvent(..)
                            , IsAcidic(..), makeAcidic, openLocalState
                            )
import Data.Acid.Local      ( createCheckpointAndClose
                            , openLocalStateFrom
                            )
import Data.Acid.Advanced   (query', update')
import Data.IxSet           ( Indexable(..), IxSet(..), (@=), Proxy(..), getOne
                            , ixFun, ixSet )
import qualified Data.IxSet as IxSet

newtype UserId = UserId { unUserId :: Integer }
    deriving (Eq, Ord, Data, Enum, Typeable, SafeCopy)

data User = User
    { _userId   :: UserId
    , _email    :: Text
    , _name     :: Text
    , _password :: Text
    }
    deriving (Ord, Eq, Data, Typeable)

newtype Email = Email Text deriving (Eq, Ord, Data, Typeable, SafeCopy)
newtype Name = Name Text deriving (Eq, Ord, Data, Typeable, SafeCopy)
newtype Password = Password Text deriving (Eq, Ord, Data, Typeable, SafeCopy)

$(deriveSafeCopy 0 'base ''User)
$(makeLens ''User)


instance Indexable User where
    empty = ixSet [ ixFun $ \us -> [ userId  ^$ us ]
                  , ixFun $ \us -> [ Email  $ email ^$ us  ]
                  , ixFun $ \us -> [ Name $ name ^$ us ]
                  ]


setEmail :: Text -> Update User Text
setEmail newEmail = email != newEmail

$(makeAcidic ''User ['setEmail])

data Acid = Acid
    { acidUser :: AcidState (IxSet User) } 
