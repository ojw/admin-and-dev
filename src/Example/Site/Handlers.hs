{-# LANGUAGE DeriveDataTypeable, FlexibleContexts, GeneralizedNewtypeDeriving
  , MultiParamTypeClasses, OverloadedStrings, ScopedTypeVariables
  , TemplateHaskell, TypeFamilies, FlexibleInstances, RecordWildCards
  , TypeOperators #-}

module Example.Site.Handlers 

where

import Control.Monad.Trans          ( lift )
import Data.Acid                    ( AcidState(..) )
import Data.Acid.Advanced           ( query', update' )
import Happstack.Server.RqData
import Happstack.Server             ( Response, ServerPart, ServerPartT, ok
                                    , toResponse, simpleHTTP, nullConf
                                    , seeOther, dir, notFound, seeOther
                                    , Method(..), methodM, FilterMonad
                                    , WebMonad, ServerMonad, Happstack
                                    , mapServerPartT )
import Web.Routes                   ( RouteT, showURL )

import Text.Blaze.Html5.Attributes as A hiding (label)
import qualified Text.Blaze.Html5 as H
import Text.Blaze.Html5           as H hiding (fieldset, ol, li, label, head)
import qualified Text.Blaze.Html5.Attributes as A
import Text.Reform.Blaze.Text     as R
import Text.Reform.Happstack      as R

-- these should not be here!
import Text.Reform hiding ( (<>) )
import Happstack.Auth.Blaze.Templates
import Happstack.Auth.Core.Auth
import Happstack.Auth.Core.AuthParts
import Happstack.Auth.Core.AuthURL
import Happstack.Auth.Core.ProfileURL
import Happstack.Auth.Core.Profile
import Happstack.Auth.Core.ProfileParts
import Happstack.Auth.Core.AuthProfileURL (AuthProfileURL(..))
import           Data.Set         (Set)
import qualified Data.Set         as Set

import Util.HasAcidState
import Server.Auth.Auth
import Client.Html.Room
import Client.Html.Lobby
import Example.Acid
import Example.App
import Example.Site.Sitemap

jQueryUI = "//ajax.googleapis.com/ajax/libs/jqueryui/1.8.23/jquery-ui.min.js"
jQuery = "//ajax.googleapis.com/ajax/libs/jquery/1.8.1/jquery.min.js"

template :: String -> H.Html -> H.Html -> H.Html
template title headers body =
    H.docTypeHtml $ do
      H.head $ do
        H.title (H.toHtml title)
        H.link ! A.href "css" ! type_ "text/css" ! rel "stylesheet"
      H.body $ do
        H.h1 $ "Admin and Dev"
        H.p  $ "Serving multiplayer games."
        body
        H.script ! A.src jQuery $ "For real."
        H.script ! A.src jQueryUI $ "Srsly."
        H.script ! A.src "js" $ "This site really needs javascript."

appTemplate :: (Happstack m) => String -> Html -> Html -> m Response
appTemplate title headers body =
    ok $ toResponse $ template title headers body

welcomeBox :: Maybe UserId -> RouteT Sitemap App Html
welcomeBox Nothing = 
        do  loginURL <- showURL Login
            createURL <- showURL Create
            return $ H.a ! A.href (toValue loginURL) $ "Log in."
welcomeBox (Just u)  = 
        do  
            --something <- query' profileState GetProfileState
            logoutURL <- showURL Logout
            return $ do H.p $ H.toHtml $ show u
                        H.p $ H.a ! A.href (toValue logoutURL) $ "Log out."

homePage :: RouteT Sitemap App Response
homePage =
        do  (authState :: AcidState AuthState) <- lift getAcidState
            (profileState :: AcidState ProfileState) <- lift getAcidState
            mUserId <- getUserId authState profileState
            welcome <- welcomeBox mUserId
            appTemplate "Home" mempty $ do
                welcome
                --roomBox''
                --chatBox
                lobby

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
           e <- happstackEitherForm (R.form actionURL) "naf" (Server.Auth.Auth.newAccountForm authStateH profileStateH)
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


