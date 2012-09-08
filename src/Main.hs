{-# LANGUAGE DeriveDataTypeable, FlexibleContexts, GeneralizedNewtypeDeriving
  , MultiParamTypeClasses, OverloadedStrings, ScopedTypeVariables
  , TemplateHaskell, TypeFamilies, FlexibleInstances, RecordWildCards
  , TypeOperators #-}

module Main 

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

import Text.Blaze.Html5 hiding      ( base )
import Text.Blaze.Html5.Attributes as A hiding (label)
import qualified Text.Blaze.Html5 as H
import Text.Blaze.Html5           as H hiding (fieldset, ol, li, label, head)
import qualified Text.Blaze.Html5.Attributes as A
import Text.Reform.Blaze.Text     as R
import Text.Reform.Happstack      as R

import HasAcidState
-- import Authentication

import Text.Reform

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

-- this example is not really necessary since I've taken most wisdom
-- from it and put it in the Authenticaion module

data Sitemap
    = Home
--    | U_Profile UserId
    | Create
--    | Echo Text
      deriving (Eq, Ord, Read, Show, Data, Typeable)

$(derivePrinterParsers ''Sitemap)

sitemap :: Router () (Sitemap :- ())
sitemap =
    (  rHome
--    Web.Routes.Boomerang.<> rProfile . (lit "profile" </> userId)
    Web.Routes.Boomerang.<> rCreate . (lit "create")
--    Web.Routes.Boomerang.<> rEcho . (lit "message" </> anyText)
    )
--    where userId :: Router () (UserId :- ())
--          userId = 
--              xmaph UserId (Just . _unUserId) integer

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
        

route :: Sitemap -> RouteT Sitemap App Response -- (ServerPartT IO) Response
route url =
    case url of
      Home              -> 
        do (authStateH :: AcidState AuthState) <- lift getAcidState
           (profileState :: AcidState ProfileState) <- lift getAcidState
           createURL <- showURL Create -- fix later
           actionURL <- showURL Home -- same
           onAuthURL <- showURL Home -- ugh
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
                    seeOther (Text.unpack onAuthURL) (toResponse ())
          --  ok $ toResponse $ template "Title" [] $ loginForm authState here-- "FOO" --loginBox
--      (U_Profile userId)  -> ok $ toResponse $ template "Profile" [] $ H.toHtml $ "Profile: " ++ show (_unUserId userId)
--      (Echo message)    -> ok $ toResponse $ template "Message" [] $ H.toHtml $ "Message: " ++ unpack message
--      Create -> appTemplate "Create" mempty mempty
      Create -> 
        do (authStateH :: AcidState AuthState) <- lift getAcidState
           actionURL <- showURL Create
           onAuthURL <- showURL Home
           e <- happstackEitherForm (R.form actionURL) "naf" (newAccountForm authStateH)
           case e of
             (Left formHtml) ->
                 do r <- appTemplate "Create New Account" mempty $
                     H.div ! A.id "happstack-authenticate" $
                      do h1 "Create an account"
                         formHtml
                    ok r

             (Right (authId, userPassId)) ->
                do addAuthCookie authStateH (Just authId) (AuthUserPassId userPassId)
                   seeOther (Text.unpack onAuthURL) (toResponse ())

site :: Site Sitemap (App Response) -- (ServerPartT IO Response)
site =
       setDefault Home $ boomerangSite (runRouteT route) sitemap


------------------------------------------------------------------------

data AuthTemplateError
    = ATECommon (CommonFormError [Input])
    | UPE UserPassError
    | MinLength Int
    | PasswordMismatch

instance FormError AuthTemplateError where
    type ErrorInputType AuthTemplateError = [Input]
    commonFormError = ATECommon

--instance ToMarkup (CommonFormError [Input]) where
--    toMarkup e = toMarkup $ show e

instance ToMarkup AuthTemplateError where
    toMarkup (ATECommon e)    = toHtml $ e
    toMarkup (UPE e)          = toHtml $ userPassErrorString e
    toMarkup (MinLength n)    = toHtml $ "mimimum length: " ++ show n
    toMarkup PasswordMismatch = "Passwords do not match."

type AuthForm m a = Form m [Input] AuthTemplateError Html () a



------------------------------------------------------------------------

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

----------------------------------------------------------------------

-- gatherers the acid types from each plugin
-- data Acid = Acid { acidAuthState    :: AcidState AuthenticationState }
data Acid = Acid
     { acidAuth        :: AcidState AuthState
     , acidProfile     :: AcidState ProfileState
--     , acidProfileData :: AcidState ProfileDataState
     }

newtype App a = App { unApp :: ServerPartT (ReaderT Acid IO) a }
    deriving ( Functor, Alternative, Applicative, Monad, MonadPlus, MonadIO
               , HasRqData, ServerMonad ,WebMonad Response, FilterMonad Response
               , Happstack, MonadReader Acid)

-- assigns directories to each acid type
{-
withAcid :: Maybe FilePath -> (Acid -> IO a) -> IO a
withAcid mBasePath action =
    let basePath = fromMaybe "_state" mBasePath
    in withLocalState (Just $ basePath FP.</> "auth") initialAuthState $ \c ->        
           action (Acid c) 
-}

withAcid :: Maybe FilePath -- ^ state directory
         -> (Acid -> IO a) -- ^ action
         -> IO a
withAcid mBasePath f =
    let basePath = fromMaybe "_state" mBasePath in
    bracket (openLocalStateFrom (basePath F.</> "auth")        initialAuthState)        (createCheckpointAndClose) $ \auth ->
    bracket (openLocalStateFrom (basePath F.</> "profile")     initialProfileState)     (createCheckpointAndClose) $ \profile ->
--     bracket (openLocalStateFrom (basePath </> "profileData") initialProfileDataState) (createCheckpointAndClose) $ \profileData ->
        f (Acid auth profile) --profileData)


runApp :: Acid -> App a -> ServerPartT IO a
runApp acid (App sp) = mapServerPartT (flip runReaderT acid) sp

-- need instance for each acid type
instance HasAcidState App AuthState where
    getAcidState = acidAuth    <$> ask 

instance HasAcidState App ProfileState where
    getAcidState = acidProfile    <$> ask 

main :: IO ()
main = withAcid Nothing $ \acid -> simpleHTTP nullConf $ runApp acid serverPart

-- puts each plugin on its own subdirectory
serverPart:: App Response -- ServerPartT IO Response
serverPart =  
    do  decodeBody (defaultBodyPolicy "/tmp/" 0 1000 1000)
        msum [ Happstack.Server.dir "favicon.ico" $ notFound (toResponse ())
             , implSite "http://localhost:8000" "/route" site
           --, implSite "http://localhose:8000" "/auth" (authSite template)
             , seeOther ("/route/" :: String) (toResponse ())
             ]
