{-# LANGUAGE DeriveDataTypeable, TemplateHaskell, GeneralizedNewtypeDeriving #-}

module Framework.Auth.Internal.Types.UserPassword where

import Data.Lens
import Data.Lens.Template
import Data.Data
import Data.SafeCopy
import Data.IxSet
import Data.ByteString.Char8 ( ByteString )
import Framework.Profile ( UserId )

newtype HashedPass = HashedPass ByteString deriving (Ord, Eq, Read, Show, Data, Typeable, SafeCopy)

data UserPassword = UserPassword
    { _userId   :: UserId
    , _password :: HashedPass
    } deriving (Ord, Eq, Read, Show, Data, Typeable)

$(makeLens ''UserPassword)
$(deriveSafeCopy 0 'base ''UserPassword)
$(inferIxSet "UserPasswords" ''UserPassword 'noCalcs [''UserId, ''HashedPass])
