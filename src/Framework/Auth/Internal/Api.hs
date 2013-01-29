

module Framework.Auth.Internal.Api where

import Data.Text                    ( Text )
import Data.ByteString.Lazy.Char8   ( ByteString )

newtype PlainPass = PlainPass Text  deriving (Ord, Eq, Read, Show)
newtype Email = Email Text          deriving (Ord, Eq, Read, Show)
newtype Username = Username Text    deriving (Ord, Eq, Read, Show)

newtype AuthToken = ByteString

data AuthApi
    = Register Username Email PlainPass
    | Authenticate (Either Username Email) PlainPass
    | UpdatePassword (Either Username Email) PlainPass PlainPass

data AuthError
    = UsernameNotAvailable
    | EmailNotAvailable
    | IncorrectUsernameOrPassword

data AuthView
    = AuthTokenView AuthToken
