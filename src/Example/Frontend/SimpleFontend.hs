module Example.Fontend.SimpleFontend where

import Control.Applicative hiding ((<|>), many)
import Text.Parsec.ByteString.Lazy
import Text.Parsec.Char
import Text.Parsec.Combinator
import Text.Parsec.Prim
import Data.Text as Text
import Data.ByteString.Lazy as ByteString
import Data.ByteString.Lazy.Char8 as Char8

import Framework.Location.Api
import Framework.Auth.Api
import Framework.Api
import Framework.View
import Common.Auth.Types hiding ( authToken )
import Common.Profile.Types hiding ( email ) 
import Common.Location.Types hiding ( locationId )

word :: Parser String
word = many1 letter

number :: Parser String
number = many1 digit

email :: Parser Email
email = Email . Text.pack <$> word

plainPass :: Parser PlainPass
plainPass = PlainPass . Char8.pack <$> word

authToken :: Parser AuthToken
authToken = AuthToken . Char8.pack <$> word

text :: Parser Text
text = Text.pack <$> word

locationId :: Parser LocationId
locationId = 
        InLobby . LobbyId . read <$> (string "lobby" *> number)
    <|> InMatchmaker . MatchmakerId . read <$> (string "matchmaker" *> number)
    <|> InGame . GameId . read <$> (string "game" *> number)

locationApi :: Parser LocationApi
locationApi = 
        Join <$> (string "join" *> locationId)
    <|> Leave <$ string "leave"
    <|> Look <$> (string "look" *> locationId)
    <|> Chat . Text.pack <$> (string "chat" *> string "|" *> many word *> string "|") <*> locationId -- this only parses one-word chats... clearly not the correct behavior!
--    <|> Create <$ string "create"

authApi :: Parser AuthApi
authApi =
        string "register" *> (Register <$> text <*> email <*> plainPass)
    <|> string "login" *> (LogIn <$> text <*> plainPass)
    <|> string "update password" *> (UpdatePassword <$> text <*> plainPass <*> plainPass)
    <|> string "log out" *> (LogOut <$> text)

internalApi :: Parser InternalApi
internalApi =
        FWLocApi <$> locationApi
    <|> FWAuthApi <$> authApi

frameworkApi :: Parser FrameworkApi
frameworkApi = FrameworkApi <$> optionMaybe (string "token" *> authToken) <*> (string "api" *> internalApi)

decodeApi :: ByteString -> Maybe FrameworkApi
decodeApi byteString = 
    case parse frameworkApi "" byteString of 
        Left error -> Nothing
        Right api -> Just api

encodeView :: FrameworkView -> ByteString
encodeView view = Char8.pack "view"

simpleFrontEnd = Frontend
    { apiDecoder = decodeApi
    , viewEncoder = encodeView
    }
