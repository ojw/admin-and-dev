

module Framework.Auth.Internal.Api where

import Data.Text ( Text )
import Data.ByteString.Char8 ( ByteString )
import Framework.Profile
import Framework.Auth.Internal.Types.UserPassword
import Framework.Auth.Internal.Types.UserToken

newtype PlainPass = PlainPass Text  deriving (Ord, Eq, Read, Show)

data AuthApi
    = Register UserName Email PlainPass
    | Authenticate Text PlainPass
    | UpdatePassword Text PlainPass PlainPass

data AuthError
    = UserNameNotAvailable
    | EmailNotAvailable
    | IncorrectUserNameOrPassword
    | UserDoesNotExist

data AuthView
    = AuthTokenView AuthToken
