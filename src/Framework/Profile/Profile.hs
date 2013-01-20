module Framework.Profile.Profile where

import Data.Text            ( Text )
import Framework.Auth.Auth  ( UserId )

class Profile p where
    userId      :: p -> UserId
    userName    :: p -> Text
    isAdmin     :: p -> Bool
