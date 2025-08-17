module Madf.Blog
    ( serve
    , routes
    ) where

import Control.Monad
import qualified Data.Text as DT
import qualified Data.Text.Lazy as DTL
import Data.Text.Encoding
import Data.Maybe
import Data.Pool
import qualified Data.Aeson as DA
import Database.SQLite.Simple
import Control.Monad.Reader
import Web.Scotty.Trans as WS
import Web.Scotty.Cookie
import qualified Network.HTTP.Types as NT
import Network.Wai.Middleware.Static
import Network.Wai.Middleware.Cors
import Network.Wai.Middleware.RequestLogger
import qualified Madf.Blog.Post.Storage as PostStorage
import qualified Madf.Blog.Post.View as PostView
import qualified Madf.Blog.Image as Image
import qualified Madf.Blog.Env as Env
import Madf.Blog.Config
import qualified Madf.Blog.Pages as Pages
import qualified Madf.Blog.Pages.Edit as Pages
import qualified Madf.Blog.Pages.Preview as Pages
import qualified Madf.Blog.Pages.Login as Pages
import qualified Madf.Blog.Pages.Template as Template
import qualified Madf.Blog.DB as DB
import qualified Madf.Blog.JWT as JWT
import qualified Madf.Blog.Login as Login
import qualified Madf.Blog.Auth as Auth
import qualified Madf.Blog.Slug as Slug
import Madf.Blog.Publish
import Madf.Blog.Utils
import Lucid

type App a = ScottyT Env.EnvM a
type Action a = ActionT Env.EnvM a

askPool :: Action (Pool Connection)
askPool = lift $ asks Env.pool

withConn :: (Connection -> IO a) -> Action a
withConn f = do
    pool <- askPool
    liftIO $ withResource pool f

askConfig :: Action Config
askConfig = lift $ asks Env.config

serve :: Env.Env -> IO ()
serve env = do
    withResource (Env.pool env) DB.check
    scottyOptsT WS.defaultOptions (Env.runIO env) (routes base)
    where
        base = urlBase . main . Env.config $ env

routes :: DT.Text -> App ()
routes base = do
    middleware logStdoutDev
    middleware $ staticPolicy (noDots >-> addBase "static")
    middleware $ cors $ const $ Just simpleCorsResourcePolicy
        { corsRequestHeaders = "Authorization":simpleHeaders
        , corsMethods = "PUT":"DELETE":simpleMethods
        }
    options (regex ".*") $ return ()
    pages
    blog base
    api
    get "/api/health" $ json True

lucid :: Html a -> Action ()
lucid h = do
    setHeader "Content-Type" "text/html"
    raw (renderBS h)

showPage :: Html () -> Action ()
showPage p = do
    cy <- liftIO currentYear
    lucid $ Template.admin cy p

jsonError :: DT.Text -> Action ()
jsonError = json

pages :: App ()
pages = do
    get    "/admin" $ do
        Auth.require
        page <- queryParamMaybe "page"
        perPage <- queryParamMaybe "perPage"
        posts <- withConn $ \conn -> PostView.list conn (fromMaybe 0 page) (fromMaybe 10 perPage)
        showPage $ Pages.mainPage posts
    get    "/admin/login" $ do
        me <- queryParamMaybe "error"
        showPage $ Pages.loginPage me
    post   "/admin/login" $ do
        f <- queryParamMaybe "from"
        l <- formParam "login"
        p <- formParam "password"
        conf <- askConfig
        if Login.verify (admin conf) l p
            then do
                jwtEnv <- lift $ asks Env.jwt
                t <- liftIO $ JWT.issue jwtEnv
                setCookie $ defaultSetCookie { setCookieName = "authtoken", setCookieValue = encodeUtf8 t, setCookiePath = Just "/" }
                redirect (fromMaybe "/admin" f)
            else Auth.redirectUnauthorized (DTL.toStrict <$> f) "Bad credentials"
    get    "/admin/new" $ do
        Auth.require
        r <- withConn $ \conn -> PostStorage.create conn
        redirect $ "/admin/edit/" <> (DTL.fromStrict . Slug.unSlug . PostStorage.postSlug $ r)
    get    "/admin/edit/:postSlug" $ do
        Auth.require
        i <- pathParam "postSlug"
        mp <- withConn $ \conn -> PostView.get conn i
        case mp of
            Just p -> showPage $ Pages.editPage p
            Nothing -> do
                status NT.notFound404
                showPage $ Pages.notFound "Unknown post id"
    get    "/admin/preview/:postSlug" $ do
        Auth.require
        i <- pathParam "postSlug"
        mp <- withConn $ \conn -> PostView.get conn i
        case mp of
            Just p -> showPage $ Pages.preview False p
            Nothing -> do
                status NT.notFound404
                showPage $ Pages.notFound "Unknown post id"

blog :: DT.Text -> App ()
blog base = do
    get (capture . DT.unpack $ "/" <> base <> "/:year/:fileName") $ do
        year <- pathParam "year"
        fn <- pathParam "fileName"
        conf <- askConfig
        let dd = destDir (main conf)
        setHeader "Content-Type" "text/html"
        file (DT.unpack $ dd <> "/" <> year <> "/" <> fn)
    get (capture . DT.unpack $ "/" <> base <> "/:year/:postSlug/:fileName") $ do
        year <- pathParam "year"
        slug <- pathParam "postSlug"
        fn <- pathParam "fileName"
        mi <- withConn $ \conn -> Image.getByFileName conn slug fn
        conf <- askConfig
        let dd = destDir (main conf)
        case mi of
            Nothing -> status NT.notFound404
            Just img -> do
                setHeader "Content-Type" (DTL.fromStrict $ Image.imageMIMEType img)
                file (DT.unpack $ dd <> "/" <> year <> "/" <> Slug.unSlug slug <> "/" <> fn)

api :: App ()
api = do
    loginAPI
    imageAPI
    postAPI

loginAPI :: App ()
loginAPI = do
    post "/admin/api/token/issue" $ do
        l <- formParam "login"
        p <- formParam "password"
        conf <- askConfig
        if Login.verify (admin conf) l p then do
            jwtEnv <- lift $ asks Env.jwt
            t <- liftIO $ JWT.issue jwtEnv
            json t
             else status NT.unauthorized401 >> jsonError "Bad credentials"
    post "/admin/api/token/renew" $ do
        mt <- getCookie "authtoken"
        case mt of
            Nothing -> status NT.unauthorized401 >> jsonError "No authorization token"
            Just t -> do
                jwtEnv <- lift $ asks Env.jwt
                nt <- liftIO $ JWT.renew jwtEnv t
                setCookie $ defaultSetCookie { setCookieName = "authtoken", setCookieValue = encodeUtf8 nt, setCookiePath = Just "/" }
                json nt

imageAPI :: App ()
imageAPI = do
    get    "/admin/api/image/:imageId" $ do
        Auth.requireNoRedirect
        iid <- pathParam "imageId"
        r <- withConn (`Image.get` iid)
        json r
    put    "/admin/api/image/:imageId" $ do
        Auth.requireNoRedirect
        i <- pathParam "imageId"
        c <- formParam "caption"
        r <- withConn  $ \conn -> Image.updateCaption conn i c
        json r
    delete "/admin/api/image/:imageId" $ do
        Auth.requireNoRedirect
        iid <- pathParam "imageId"
        withConn (`Image.delete` iid)

postAPI :: App ()
postAPI = do
    get    "/admin/api/post/:postSlug" $ do
        Auth.requireNoRedirect
        slug <- pathParam "postSlug"
        r <- withConn $ \conn -> PostView.get conn slug
        json r
    put    "/admin/api/post/:postSlug" $ do
        Auth.requireNoRedirect
        s <- pathParam "postSlug"
        t <- formParam "title"
        c <- formParam "content"
        ty <- formParam "type"
        r <- formParam "reason"
        d <- formParam "draft"
        conf <- askConfig
        case DA.eitherDecode c of
            Right bs -> withConn $ \conn -> do
                PostView.update conn s t bs (PostStorage.makeType ty r) d
                unless d (liftIO $ publish conn conf s)
            Left m -> status NT.badRequest400 >> json m
    post   "/admin/api/post/:postSlug/image" $ do
        Auth.requireNoRedirect
        i <- pathParam "postSlug"
        fs <- files
        conf <- askConfig
        r <- withConn $ \conn -> mapM (Image.upload conn conf i) fs
        json r
