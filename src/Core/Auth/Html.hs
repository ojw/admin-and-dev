{-# LANGUAGE DeriveDataTypeable, FlexibleContexts, GeneralizedNewtypeDeriving
  , MultiParamTypeClasses, OverloadedStrings, ScopedTypeVariables
  , TemplateHaskell, TypeFamilies, FlexibleInstances, RecordWildCards
  , TypeOperators #-}

module Core.Auth.Html

( loginForm
, Core.Auth.Html.newAccountForm
)

where

import Control.Applicative          ( (<$>), (<*>), Applicative, Alternative )
import Control.Applicative          ( (<*), (*>), optional)
import Control.Monad.Trans          ( lift, MonadIO(..) )
import Data.Acid                    ( AcidState(..), EventState(..)
                                    , EventResult(..) , Query(..)
                                    , QueryEvent(..), Update(..)
                                    , UpdateEvent(..), IsAcidic(..), makeAcidic
                                    , openLocalState)
import Data.Acid.Advanced           ( query', update' )
import Data.Acid.Local              ( createCheckpointAndClose 
                                    , openLocalStateFrom)
import Data.Text as Text            ( Text, unpack, reverse, toUpper, length )
import Data.Text.Encoding           ( encodeUtf8 )
import Happstack.Server             ( Response, ServerPart, ServerPartT, ok
                                    , toResponse, simpleHTTP, nullConf
                                    , seeOther, dir, notFound, seeOther
                                    , Method(..), methodM, FilterMonad
                                    , WebMonad, ServerMonad, Happstack
                                    , mapServerPartT )
import Happstack.Server             ( Input, internalServerError, toResponse, unauthorized )
import System.FilePath as FP
import Text.Boomerang.TH            ( derivePrinterParsers )

import Text.Blaze.Html5.Attributes as A hiding (label)
import qualified Text.Blaze.Html5 as H
import Text.Blaze.Html5           as H hiding (fieldset, ol, li, label, head)
import qualified Text.Blaze.Html5.Attributes as A

import Text.Reform.Blaze.Text     as R
import Text.Reform.Happstack      as R

import Text.Reform hiding ( (<>) )

import Happstack.Auth.Blaze.Templates

import Happstack.Auth.Core.Auth
import Happstack.Auth.Core.AuthParts
import Happstack.Auth.Core.AuthURL
import Happstack.Auth.Core.ProfileURL
import Happstack.Auth.Core.Profile
import Happstack.Auth.Core.ProfileParts
import Happstack.Auth.Core.AuthProfileURL (AuthProfileURL(..))
 
import Control.Exception           (bracket)

import           Data.Set         (Set)
import qualified Data.Set         as Set

import Util.HasAcidState
import Data.Aeson

----------------------------------------------------------------------

data AuthTemplateError
    = ATECommon (CommonFormError [Input])
    | UPE UserPassError
    | MinLength Int
    | PasswordMismatch

instance FormError AuthTemplateError where
    type ErrorInputType AuthTemplateError = [Input]
    commonFormError = ATECommon

instance ToMarkup AuthTemplateError where
    toMarkup (ATECommon e)    = toHtml $ e
    toMarkup (UPE e)          = toHtml $ userPassErrorString e
    toMarkup (MinLength n)    = toHtml $ "mimimum length: " ++ show n
    toMarkup PasswordMismatch = "Passwords do not match."

type AuthForm m a = Form m [Input] AuthTemplateError Html () a

------------------------------------------------------------------------

loginForm :: (Functor m, MonadIO m, ToValue a) 
          => AcidState AuthState
          -> a
          -> Form m [Input] AuthTemplateError Html () UserPassId
loginForm authStateH createURL =
           R.fieldset $
            (errorList ++>
              R.ol (((,) <$> (R.li $ errorList ++> R.label ("username: " :: String) ++> inputText mempty)
                         <*> (R.li $ errorList ++> R.label ("password: " :: String) ++> inputPassword)
                         <* login) `transformEitherM` checkAuth)
                   <* (create createURL))
     where

       create createURL = view $ p $ do "or "
                                        H.a ! href (toValue createURL) $ "create a new account"
       login = R.li $ mapView (\html -> html ! A.class_  "submit") (inputSubmit "Login")

       checkAuth :: (MonadIO m) => (Text, Text) -> m (Either AuthTemplateError UserPassId)
       checkAuth (username, password) =
               do r <- query' authStateH (CheckUserPass username password)
                  case r of
                    (Left e) -> return (Left $ UPE e)
                    (Right userPassId) -> return (Right userPassId) 


newAccountForm :: (Functor v, MonadIO v) => AcidState AuthState -> AcidState ProfileState -> AuthForm v (AuthId, UserPassId)
newAccountForm authStateH profileStateH =
    (R.fieldset
     (errorList ++>
      (R.ol $ (((,) <$> username <*> password <* submitButton)))
                    `transformEitherM`
                    createAccount))
    where
      submitButton = R.li $ (mapView (\html -> html ! A.class_  "submit") $ inputSubmit "Create Account")
      username  = R.li $ errorList ++> ((label ("username: " :: String)       ++> inputText mempty) `transformEither` (minLength 1))
      password1 = R.li $ label ("password: " :: String)         ++> inputPassword
      password2 = R.li $ label ("confirm password: " :: String) ++> inputPassword

      password = errorList ++> (((,) <$> password1 <*> password2) `transformEither` samePassword) `transformEither` minLength 6

      samePassword (p1, p2) =
              if p1 /= p2
               then (Left $ PasswordMismatch)
               else (Right p1)

      createAccount (username, password) =
              do passHash <- liftIO $ mkHashedPass password
                 r <- update' authStateH $ CreateUserPass (UserName username) passHash
                 case r of
                    (Left e) -> return (Left $ UPE e)
                    (Right userPass) ->
                        do authId <- update' authStateH (NewAuthMethod (AuthUserPassId (upId userPass)))
                           userId <- update' profileStateH GenUserId
                           update' profileStateH $ SetAuthIdUserId authId userId
                           return (Right (authId, upId userPass))

minLength :: Int -> Text -> Either AuthTemplateError Text
minLength n s =
          if Text.length s >= n
          then (Right s)
          else (Left $ MinLength n)
