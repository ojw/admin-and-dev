
module Core.Types 

( UserId(..)
, GameType
)

where

import Data.Data
import Data.SafeCopy
import Core.Auth.Acid   ( UserId(..) ) -- this is horrible and I hate it

data GameType = GamePlaceholder

newtype GenericId = { unGenericId :: Int } deriving (Ord, Eq, Read, Show, Data, Typeable, Indexable, SafeCopy)
