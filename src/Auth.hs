{-# LANGUAGE DeriveDataTypeable, FlexibleContexts, GeneralizedNewtypeDeriving
  , MultiParamTypeClasses, OverloadedStrings, ScopedTypeVariables
  , TemplateHaskell, TypeFamilies, FlexibleInstances, RecordWildCards
  , TypeOperators #-}

module Auth 

( loginForm
, newAccountForm
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
import Data.Text                    ( Text, unpack, reverse, toUpper )
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

import Text.Blaze.Html5 hiding      ( base )
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
