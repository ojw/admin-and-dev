{-# LANGUAGE DeriveDataTypeable, FlexibleContexts, GeneralizedNewtypeDeriving
  , MultiParamTypeClasses, OverloadedStrings, ScopedTypeVariables
  , TemplateHaskell, TypeFamilies, FlexibleInstances, RecordWildCards
  , TypeOperators #-}

module Pages 

where

import Control.Applicative          ( (<$>), (<*>), Applicative, Alternative )
import Control.Applicative          ( (<*), (*>), optional)
import Control.Category             ( (.) )
import Control.Monad                ( msum, liftM, MonadPlus )
import Control.Monad.Reader         ( ask, ReaderT(..), MonadReader )
import Control.Monad.State          ( get, put, gets )
import Control.Monad.Trans          ( lift, MonadIO(..) )
import Crypto.BCrypt                ( validatePassword, hashPasswordUsingPolicy
                                    , slowerBcryptHashingPolicy )
import Data.Maybe                   ( fromMaybe, fromJust )
import Data.SafeCopy                ( SafeCopy, base, deriveSafeCopy )
import Data.Lens.Template           ( makeLens )
import Data.Acid                    ( AcidState(..), EventState(..)
                                    , EventResult(..) , Query(..)
                                    , QueryEvent(..), Update(..)
                                    , UpdateEvent(..), IsAcidic(..), makeAcidic
                                    , openLocalState)
import Data.Acid.Advanced           ( query', update' )
import Data.Acid.Local              ( createCheckpointAndClose 
                                    , openLocalStateFrom)
import Data.ByteString              ( ByteString, pack )
import Data.Data                    ( Data, Typeable )
import Data.Functor                 ( (<$>) )
import Data.IxSet                   ( Indexable(..), IxSet(..), (@=), Proxy(..)
                                    , getOne, ixFun, updateIx, size, null
                                    , ixSet )
import Data.Lens                    ( (%=), (!=), (^$), (^=), Lens )
import Data.Lens.IxSet              ( ixLens )
import Data.Map                     ( Map, insert, adjust, lookup )
import Data.Text                    ( Text, unpack, reverse, toUpper )
import qualified Data.Text as Text
import Data.Text.Encoding           ( encodeUtf8 )
import Happstack.Server.RqData
import Happstack.Server             ( Response, ServerPart, ServerPartT, ok
                                    , toResponse, simpleHTTP, nullConf
                                    , seeOther, dir, notFound, seeOther
                                    , Method(..), methodM, FilterMonad
                                    , WebMonad, ServerMonad, Happstack
                                    , mapServerPartT )
import Happstack.Server            (Input, internalServerError, toResponse, unauthorized)
import Prelude  hiding              ( null, (.) )
import qualified System.FilePath as FP
import Text.Boomerang.TH            ( derivePrinterParsers )
import Web.Routes                   ( RouteT, runRouteT, Site(..), setDefault
                                    , MonadRoute, askRouteFn, URL, PathInfo, showURL )
import Web.Routes.Boomerang         ( (<>), lit, (</>), anyText, (:-), Router
                                    , xmaph, int, boomerangSite, integer )
import Web.Routes.Happstack         ( implSite )
import Web.Routes.TH                ( derivePathInfo )

import Text.Blaze.Html5.Attributes as A hiding (label)
import qualified Text.Blaze.Html5 as H
import Text.Blaze.Html5           as H hiding (fieldset, ol, li, label, head)
import qualified Text.Blaze.Html5.Attributes as A
import Text.Reform.Blaze.Text     as R
import Text.Reform.Happstack      as R

import HasAcidState

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

import qualified System.FilePath as F        ((</>))

import           Data.Set         (Set)
import qualified Data.Set         as Set

import Auth
import Acid
import Sitemap

template :: String -> H.Html -> H.Html -> H.Html
template title headers body =
    H.docTypeHtml $ do
      H.head $ do
        H.title (H.toHtml title)
      H.body $ do
        H.h1 $ H.toHtml ("Admin and Dev" :: String)
        H.p  $ H.toHtml ("A division of Jolly Crouton Media?  Maybe?  Idk." :: String)
        body

appTemplate :: (Happstack m) => String -> Html -> Html -> m Response
appTemplate title headers body =
    ok $ toResponse $ template title headers body

homePage :: RouteT Sitemap App Response
homePage =
        do (authState :: AcidState AuthState) <- lift getAcidState
           (profileState :: AcidState ProfileState) <- lift getAcidState
           mUserId <- getUserId authState profileState
           case mUserId of
            Nothing -> 
                do  loginURL <- showURL Login
                    appTemplate "Home" mempty $ H.a ! A.href (toValue loginURL) $ "Log in."
            Just u  -> 
                do  
                    --something <- query' profileState GetProfileState
                    logoutURL <- showURL Logout
                    appTemplate "Home" mempty $
                        do H.p $ H.toHtml $ show u
                           H.p $ H.a ! A.href (toValue logoutURL) $ "Log out."

loginPage :: RouteT Sitemap App Response
loginPage =
        do (authStateH :: AcidState AuthState) <- lift getAcidState
           (profileState :: AcidState ProfileState) <- lift getAcidState
           createURL <- showURL Create
           actionURL <- showURL Login
           onAuthURL <- showURL Home
           e <- happstackEitherForm (R.form actionURL) "lf" (loginForm authStateH createURL)
           case e of
             (Left errorForm) ->
                 do r <- appTemplate "Login" mempty $
                     H.div ! A.id "happstack-authenticate" $
                      do h1 "Login"
                         errorForm
                    ok r
             (Right userPassId) ->
                 do authId <- do authIds <- query' authStateH (UserPassIdAuthIds userPassId)
                                 case Set.size authIds of
                                      1 -> return (Just $ Prelude.head $ Set.toList $ authIds)
                                      n -> return Nothing
                    addAuthCookie authStateH authId (AuthUserPassId userPassId)
                    seeOther onAuthURL (toResponse ())

logoutPage :: RouteT Sitemap App Response
logoutPage =
    do  
        homeURL <- showURL Home
        authState :: AcidState AuthState <- lift getAcidState
        deleteAuthCookie authState
        seeOther homeURL (toResponse ())

createPage :: RouteT Sitemap App Response
createPage =
        do (authStateH :: AcidState AuthState) <- lift getAcidState
           (profileStateH :: AcidState ProfileState) <- lift getAcidState
           actionURL <- showURL Create
           onAuthURL <- showURL Home
           e <- happstackEitherForm (R.form actionURL) "naf" (Pages.newAccountForm authStateH profileStateH)
           case e of
             (Left formHtml) ->
                 do r <- appTemplate "Create New Account" mempty $
                     H.div ! A.id "happstack-authenticate" $
                      do h1 "Create an account"
                         formHtml
                    ok r

             (Right (authId, userPassId)) ->
                do addAuthCookie authStateH (Just authId) (AuthUserPassId userPassId)
                   seeOther onAuthURL (toResponse ()) 


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
                 liftIO $ putStrLn "FOOOOO"
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
