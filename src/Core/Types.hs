
module Types 

( UserId(..)
, GameType
)

where

import Core.Auth.Acid   ( UserId(..) ) -- this is horrible and I hate it

data GameType = GamePlaceholder
