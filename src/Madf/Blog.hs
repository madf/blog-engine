module Madf.Blog
    ( serve
    , routes
    ) where

import Control.Monad
import Data.Text qualified as DT
import Data.Text.Lazy qualified as DTL
import Data.Text.Encoding
import Data.Maybe
import Data.Pool
import Data.Aeson qualified as DA
import Control.Monad.Reader
import Web.Scotty.Trans as WS
import Web.Scotty.Cookie
import Network.HTTP.Types qualified as NT
import Network.Wai.Middleware.Static
import Network.Wai.Middleware.Cors
import Network.Wai.Middleware.RequestLogger
import Madf.Blog.App
import Madf.Blog.Post.Storage qualified as PostStorage
import Madf.Blog.Post.View qualified as PostView
import Madf.Blog.Image qualified as Image
import Madf.Blog.Env qualified as Env
import Madf.Blog.Config
import Madf.Blog.Pages qualified as Pages
import Madf.Blog.Pages.Edit qualified as Pages
import Madf.Blog.Pages.Preview qualified as Pages
import Madf.Blog.Pages.Login qualified as Pages
import Madf.Blog.Pages.Template qualified as Template
import Madf.Blog.DB qualified as DB
import Madf.Blog.JWT qualified as JWT
import Madf.Blog.Login qualified as Login
import Madf.Blog.Auth qualified as Auth
import Madf.Blog.Slug qualified as Slug
import Madf.Blog.Publish
import Madf.Blog.Time
import Madf.Blog.Public.Routes qualified as Public
import Lucid

serve :: Env.Env -> IO ()
serve env = do
    withResource (Env.pool env) DB.check
    scottyOptsT WS.defaultOptions (Env.runIO env) (routes env)

routes :: Env.Env -> App ()
routes env = do
    middleware logStdoutDev
    middleware $ staticPolicy (noDots >-> addBase "static")
    middleware $ cors $ const $ Just simpleCorsResourcePolicy
        { corsRequestHeaders = "Authorization":simpleHeaders
        , corsMethods = "PUT":"DELETE":simpleMethods
        }
    options (regex ".*") $ return ()
    pages
    Public.routes env
    api
    get "/api/health" $ json True

lucid :: Html a -> Action ()
lucid = html . renderText

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
