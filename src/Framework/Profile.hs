module Framework.Profile where

import Data.Text            ( Text )
import Framework.Auth  ( UserId )

type UserName = Text

class Profile p where
    userId      :: p -> UserId
    userName    :: p -> UserName
    isAdmin     :: p -> Bool
