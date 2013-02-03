{-# LANGUAGE DeriveDataTypeable, TemplateHaskell, GeneralizedNewtypeDeriving #-}

module Framework.Auth.Internal.Types.UserToken where

import Data.Lens
import Data.Lens.Template
import Data.Data
import Data.SafeCopy
import Data.IxSet
import Data.Text ( Text )
import Data.ByteString.Char8 ( ByteString )
import Framework.Profile

newtype AuthToken = AuthToken ByteString deriving (Ord, Eq, Read, Show, Data, Typeable, SafeCopy)

data UserToken = UserToken
    { _userId       :: UserId
    , _authToken    :: AuthToken
    } deriving (Ord, Eq, Read, Show, Data, Typeable)

$(makeLens ''UserToken)
$(deriveSafeCopy 0 'base ''UserToken)
$(inferIxSet "UserTokens" ''UserToken 'noCalcs [''UserId, ''AuthToken])
