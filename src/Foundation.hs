{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE QuasiQuotes #-}

module Foundation where

import Import.NoFoundation
import Database.Persist.Sql (ConnectionPool, runSqlPool)
import Text.Hamlet          (hamletFile)
import Text.Jasmine         (minifym)
import Control.Monad.Logger (LogSource)
import Text.Julius (rawJS)
import qualified Network.Wai as Wai

-- Used only when in "auth-dummy-login" setting is enabled.
import Yesod.Auth.Dummy

import Yesod.Auth.OpenId    (authOpenId, IdentifierType (..), forwardUrl)
import qualified Yesod.Auth.Message as AuMsg
import Yesod.Default.Util   (addStaticContentExternal)
import Yesod.Core.Types     (Logger)
import qualified Yesod.Core.Unsafe as Unsafe
import qualified Data.CaseInsensitive as CI
import qualified Data.Text.Encoding as TE
import qualified Data.Text.Encoding.Error as TEE
import qualified Data.ByteString.Char8 as S8

-- | The foundation datatype for your application. This can be a good place to
-- keep settings and values requiring initialization before your application
-- starts running, such as database connections. Every handler will have
-- access to the data present here.
data App = App
    { appSettings    :: AppSettings
    , appStatic      :: Static -- ^ Settings for static file serving.
    , appConnPool    :: ConnectionPool -- ^ Database connection pool.
    , appHttpManager :: Manager
    , appLogger      :: Logger
    }

data MenuItem = MenuItem
    { menuItemLabel :: Text
    , menuItemRoute :: Route App
    , menuItemAccessCallback :: Bool
    }

data MenuTypes
    = NavbarLeft MenuItem
    | NavbarRight MenuItem

-- This is where we define all of the routes in our application. For a full
-- explanation of the syntax, please see:
-- http://www.yesodweb.com/book/routing-and-handlers
--
-- Note that this is really half the story; in Application.hs, mkYesodDispatch
-- generates the rest of the code. Please see the following documentation
-- for an explanation for this split:
-- http://www.yesodweb.com/book/scaffolding-and-the-site-template#scaffolding-and-the-site-template_foundation_and_application_modules
--
-- This function also generates the following type synonyms:
-- type Handler = HandlerT App IO
-- type Widget = WidgetT App IO ()
mkYesodData "App" $(parseRoutesFile "config/routes")

-- | A convenient synonym for creating forms.
type Form x = Html -> MForm (HandlerFor App) (FormResult x, Widget)

-- | A convenient synonym for database access functions.
type DB a = forall (m :: * -> *).
    (MonadIO m) => ReaderT SqlBackend m a

-- Please see the documentation for the Yesod typeclass. There are a number
-- of settings which can be configured by overriding methods here.
instance Yesod App where
    -- Controls the base of generated URLs. For more information on modifying,
    -- see: https://github.com/yesodweb/yesod/wiki/Overriding-approot
    approot :: Approot App
    approot = ApprootRequest $ \app req -> (fromMaybe (getApprootText guessApproot app req) (appRoot $ appSettings app))

    -- Custom error handler
    errorHandler :: ErrorResponse -> Handler TypedContent
    errorHandler errRes = case errRes of
        NotFound -> selectRep $ do
            provideRep $ defaultLayout $ do
                setTitle "Not Found"
                $(widgetFile "errors/notFound")
            provideRep $ return $ object ["message" .= ("Not Found" :: Text)]
        NotAuthenticated -> selectRep $ do
            provideRep $ defaultLayout $ do
                setTitle "Not Authenticated"
                $(widgetFile "errors/notAuthenticated")
            provideRep $ do
                addHeader "WWW-Authenticate" "RedirectJSON realm=\"application\", param=\"authentication_url\""
                site <- getYesod
                rend <- getUrlRender
                let apair u = ["authentication_url" .= rend u]
                    content = maybe [] apair (authRoute site)
                return $ object $ ("message" .= ("Not logged in"::Text)) : content
        (PermissionDenied msg) -> selectRep $ do
            provideRep $ defaultLayout $ do
                setTitle "Permission Denied"
                $(widgetFile "errors/permissionDenied")
            provideRep $ return $ object ["message" .= ("Permission Denied. " <> msg)]
        (InvalidArgs ia) -> selectRep $ do
            provideRep $ defaultLayout $ do
                setTitle "Invalid Arguments"
                $(widgetFile "errors/invalidArgs")
            provideRep $ return $ object ["message" .= ("Invalid Arguments" :: Text), "errors" .= ia]
        (InternalError e) -> do
            $logErrorS "yesod-core" e
            selectRep $ do
                provideRep $ defaultLayout $ do
                    setTitle "Internal Server Error"
                    $(widgetFile "errors/internalError")
                provideRep $ return $ object ["message" .= ("Internal Server Error" :: Text), "error" .= e]
        (BadMethod m) -> selectRep $ do
            provideRep $ defaultLayout $ do
                setTitle "Method Not Supported"
                $(widgetFile "errors/badMethod")
            provideRep $ return $ object ["message" .= ("Bad method" :: Text), "method" .= TE.decodeUtf8With TEE.lenientDecode m]

    -- Store session data on the client in encrypted cookies,
    -- default session idle timeout is 120 minutes
    makeSessionBackend :: App -> IO (Maybe SessionBackend)
    makeSessionBackend _ = Just <$> defaultClientSessionBackend
        120    -- timeout in minutes
        "config/client_session_key.aes"

    -- Yesod Middleware allows you to run code before and after each handler function.
    -- The defaultYesodMiddleware adds the response header "Vary: Accept, Accept-Language" and performs authorization checks.
    -- Some users may also want to add the defaultCsrfMiddleware, which:
    --   a) Sets a cookie with a CSRF token in it.
    --   b) Validates that incoming write requests include that token in either a header or POST parameter.
    -- To add it, chain it together with the defaultMiddleware: yesodMiddleware = defaultYesodMiddleware . defaultCsrfMiddleware
    -- For details, see the CSRF documentation in the Yesod.Core.Handler module of the yesod-core package.
    yesodMiddleware :: ToTypedContent res => Handler res -> Handler res
    yesodMiddleware = defaultYesodMiddleware

    defaultLayout :: Widget -> Handler Html
    defaultLayout widget = do
        master <- getYesod
        mmsg <- getMessage

        muser <- maybeAuthPair
        mcurrentRoute <- getCurrentRoute

        -- Get the breadcrumbs, as defined in the YesodBreadcrumbs instance.
        (title, parents) <- breadcrumbs

        -- Define the menu items of the header.
        let menuItems =
                [ NavbarLeft MenuItem
                    { menuItemLabel = "Notes"
                    , menuItemRoute = NotesR
                    , menuItemAccessCallback = isJust muser
                    }
                , NavbarLeft MenuItem
                    { menuItemLabel = "Profile"
                    , menuItemRoute = ProfileR
                    , menuItemAccessCallback = isJust muser
                    }
                , NavbarRight MenuItem
                    { menuItemLabel = "Login"
                    , menuItemRoute = AuthR LoginR
                    , menuItemAccessCallback = isNothing muser
                    }
                , NavbarRight MenuItem
                    { menuItemLabel = "Logout"
                    , menuItemRoute = AuthR LogoutR
                    , menuItemAccessCallback = isJust muser
                    }
                ]

        let navbarLeftMenuItems = [x | NavbarLeft x <- menuItems]
        let navbarRightMenuItems = [x | NavbarRight x <- menuItems]

        let navbarLeftFilteredMenuItems = [x | x <- navbarLeftMenuItems, menuItemAccessCallback x]
        let navbarRightFilteredMenuItems = [x | x <- navbarRightMenuItems, menuItemAccessCallback x]

        -- checks cookies to see if they have been accepted
        cokCk <- lookupSession cookiesKey
        let cookiesAccepted = isJust cokCk
            acceptCookiesId = "js-acceptCookies" :: Text
            cookiesWidgetId = "js-cookiesWidget" :: Text

        -- We break up the default layout into two components:
        -- default-layout is the contents of the body tag, and
        -- default-layout-wrapper is the entire page. Since the final
        -- value passed to hamletToRepHtml cannot be a widget, this allows
        -- you to use normal widget features in default-layout.

        pc <- widgetToPageContent $ do
            addStylesheet $ StaticR css_bulma_css
            let isHomePage = Just HomeR == mcurrentRoute
                cookiesWidget = $(widgetFile "cookies/widget")
            $(widgetFile "default-layout")
        withUrlRenderer $(hamletFile "templates/default-layout-wrapper.hamlet")

    -- The page to be redirected to when authentication is required.
    authRoute
        :: App
        -> Maybe (Route App)
    authRoute _ = Just $ AuthR LoginR

    isAuthorized
        :: Route App  -- ^ The route the user is visiting.
        -> Bool       -- ^ Whether or not this is a "write" request.
        -> Handler AuthResult
    -- Routes not requiring authentication.
    isAuthorized (AuthR _) _ = return Authorized
    isAuthorized HomeR _ = return Authorized
    isAuthorized FaviconR _ = return Authorized
    isAuthorized RobotsR _ = return Authorized
    isAuthorized (StaticR _) _ = return Authorized
    isAuthorized CookiesR _ = return Authorized

    -- Routes requiring authentication and/or ownership of a note
    isAuthorized ProfileR _ = isAuthenticated
    isAuthorized NotesR _ = isAuthenticated
    isAuthorized (NoteR nid) _ = isNoteViewable nid
    isAuthorized AddNoteR _ = isAuthenticated
    isAuthorized (EditNoteR nid) _ = isNoteOwner nid
    isAuthorized (DeleteNoteR nid) _ = isNoteOwner nid
    isAuthorized (PublicNoteR nid) _ = isNoteOwner nid

    -- This function creates static content files in the static folder
    -- and names them based on a hash of their content. This allows
    -- expiration dates to be set far in the future without worry of
    -- users receiving stale content.
    addStaticContent
        :: Text  -- ^ The file extension
        -> Text -- ^ The MIME content type
        -> LByteString -- ^ The contents of the file
        -> Handler (Maybe (Either Text (Route App, [(Text, Text)])))
    addStaticContent ext mime content = do
        master <- getYesod
        let staticDir = appStaticDir $ appSettings master
        addStaticContentExternal
            minifym
            genFileName
            staticDir
            (StaticR . flip StaticRoute [])
            ext
            mime
            content
      where
        -- Generate a unique filename based on the content itself
        genFileName lbs = "autogen-" ++ base64md5 lbs

    -- What messages should be logged. The following includes all messages when
    -- in development, and warnings and errors in production.
    shouldLogIO :: App -> LogSource -> LogLevel -> IO Bool
    shouldLogIO app _source level =
        return $
        appShouldLogAll (appSettings app)
            || level == LevelWarn
            || level == LevelError

    makeLogger :: App -> IO Logger
    makeLogger = return . appLogger

-- Define breadcrumbs.
instance YesodBreadcrumbs App where
    -- Takes the route that the user is currently on, and returns a tuple
    -- of the 'Text' that you want the label to display, and a previous
    -- breadcrumb route.
    breadcrumb
        :: Route App  -- ^ The route the user is visiting currently.
        -> Handler (Text, Maybe (Route App))
    breadcrumb HomeR = return ("Home", Nothing)
    breadcrumb (AuthR _) = return ("Login", Just HomeR)
    breadcrumb ProfileR = return ("Profile", Just HomeR)
    breadcrumb NotesR = return ("Notes", Just HomeR)
    breadcrumb (NoteR noteId) = do
        maybeNote <- runDB $ get noteId
        case maybeNote of
            Just note -> case noteTitle note of
                Just title -> return (title, Just NotesR)
                _ -> return ("Your note", Just NotesR)
            _ -> return ("Note not found", Just NotesR)
    breadcrumb AddNoteR = return ("Add", Just NotesR)
    breadcrumb (EditNoteR noteId) = return ("Edit", Just $ NoteR noteId)
    breadcrumb (DeleteNoteR noteId) = return ("Delete", Just $ NoteR noteId)
    breadcrumb (PublicNoteR noteId) = return ("Change visibility", Just $ NoteR noteId)
    breadcrumb CookiesR = return ("Cookies", Just HomeR)
    breadcrumb  _ = return ("Home", Nothing)

-- How to run database actions.
instance YesodPersist App where
    type YesodPersistBackend App = SqlBackend
    runDB :: SqlPersistT Handler a -> Handler a
    runDB action = do
        master <- getYesod
        runSqlPool action $ appConnPool master

instance YesodPersistRunner App where
    getDBRunner :: Handler (DBRunner App, Handler ())
    getDBRunner = defaultGetDBRunner appConnPool

instance YesodAuth App where
    type AuthId App = UserId

    -- Where to send a user after successful login
    loginDest :: App -> Route App
    loginDest _ = HomeR
    -- Where to send a user after logout
    logoutDest :: App -> Route App
    logoutDest _ = HomeR
    -- Override the above two destinations when a Referer: header is present
    redirectToReferer :: App -> Bool
    redirectToReferer _ = True

    authenticate :: (MonadHandler m, HandlerSite m ~ App)
                 => Creds App -> m (AuthenticationResult App)
    authenticate creds = liftHandler $ runDB $ do
        x <- getBy $ UniqueUser $ credsIdent creds
        case x of
            Just (Entity uid _) -> return $ Authenticated uid
            Nothing -> Authenticated <$> insert User
                { userIdent = credsIdent creds
                , userPassword = Nothing
                }

    -- You can add other plugins like Google Email, email or OAuth here
    authPlugins :: App -> [AuthPlugin App]
    authPlugins app = authOpenId OPLocal [] : extraAuthPlugins
        -- Enable authDummy login if enabled.
        where extraAuthPlugins = [authDummy | appAuthDummyLogin $ appSettings app]

    loginHandler = do
        tm <- getRouteToParent
        app <- getYesod
        authLayout $ do
            request <- getRequest
            let useDummy = appAuthDummyLogin $ appSettings app
                openidName = "openid_identifier" :: Text
            setTitleI AuMsg.LoginTitle
            $(widgetFile "login")

-- The simplest layout, used to return widgets without the complete page in ajax requests
ajaxContentLayout :: Widget -> Handler Html
ajaxContentLayout widget = do
    pc <- widgetToPageContent widget
    withUrlRenderer [hamlet| ^{pageBody pc} |]

-- Access function to determine if a user is logged in.
isAuthenticated :: Handler AuthResult
isAuthenticated = do
    muid <- maybeAuthId
    return $ case muid of
        Nothing -> AuthenticationRequired
        Just _ -> Authorized

-- Access function to determine if a note can be viewed
isNoteViewable :: NoteId -> Handler AuthResult
isNoteViewable noteId = do
    note <- runDB $ get404 noteId
    if notePublic note then return Authorized else ownsNote note

-- Access function to determine if a user is the owner of a note
isNoteOwner :: NoteId -> Handler AuthResult
isNoteOwner noteId = do
    note <- runDB $ get404 noteId
    ownsNote note

-- utility function to check note ownership
ownsNote :: Note -> Handler AuthResult
ownsNote note = do
    muid <- maybeAuthId
    return $ case muid of
        Nothing -> AuthenticationRequired
        Just uid -> if noteUserId note == uid
            then Authorized
            else Unauthorized "You do not own this note"

-- Function to determine if a request was made with ajax
isAjaxRequest :: Handler Bool
isAjaxRequest = do
    req <- waiRequest
    let reqwith = lookup "X-Requested-With" $ Wai.requestHeaders req
    return $ reqwith == Just "XMLHttpRequest"

-- key used by session to check if cookies are accepted
cookiesKey :: Text
cookiesKey = "_COK"

instance YesodAuthPersist App

-- This instance is required to use forms. You can modify renderMessage to
-- achieve customized and internationalized form validation messages.
instance RenderMessage App FormMessage where
    renderMessage :: App -> [Lang] -> FormMessage -> Text
    renderMessage _ _ = defaultFormMessage

-- Useful when writing code that is re-usable outside of the Handler context.
-- An example is background jobs that send email.
-- This can also be useful for writing code that works across multiple Yesod applications.
instance HasHttpManager App where
    getHttpManager :: App -> Manager
    getHttpManager = appHttpManager

unsafeHandler :: App -> Handler a -> IO a
unsafeHandler = Unsafe.fakeHandlerGetLogger appLogger

-- Note: Some functionality previously present in the scaffolding has been
-- moved to documentation in the Wiki. Following are some hopefully helpful
-- links:
--
-- https://github.com/yesodweb/yesod/wiki/Sending-email
-- https://github.com/yesodweb/yesod/wiki/Serve-static-files-from-a-separate-domain
-- https://github.com/yesodweb/yesod/wiki/i18n-messages-in-the-scaffolding
