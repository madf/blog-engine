module Madf.Blog
    ( serve
    , routes
    ) where

import Data.Text.Lazy.Builder
import qualified Data.Text as DT
import qualified Data.Text.Lazy as DTL
import Data.Text.Encoding
import Data.Maybe
import Data.Pool
import qualified Data.Aeson as DA
import Database.SQLite.Simple
import Control.Monad
import Control.Monad.Reader
import Web.Scotty.Trans as WS
import Web.Scotty.Cookie
import qualified Network.HTTP.Types as NT
import Network.Wai.Middleware.Static
import Network.Wai.Middleware.Cors
import qualified Madf.Blog.Post.Storage as PostStorage
import qualified Madf.Blog.Post.View as PostView
import qualified Madf.Blog.Image as Image
import qualified Madf.Blog.Env as Env
import Madf.Blog.Config
import Madf.Blog.Ids
import qualified Madf.Blog.Pages as Pages
import qualified Madf.Blog.Pages.Preview as Pages
import qualified Madf.Blog.Pages.Template as Pages
import qualified Madf.Blog.DB as DB
import qualified Madf.Blog.JWT as JWT
import qualified Madf.Blog.Login as Login
import qualified Madf.Blog.Auth as Auth
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
    scottyOptsT WS.defaultOptions (Env.runIO env) routes

routes :: App ()
routes = do
    middleware $ staticPolicy (noDots >-> addBase "static")
    middleware $ cors $ const $ Just simpleCorsResourcePolicy
        { corsRequestHeaders = "Authorization":simpleHeaders
        , corsMethods = "PUT":"DELETE":simpleMethods
        }
    options (regex ".*") $ return ()
    pages
    postData
    api
    get "/api/health" $ json True

lucid :: Html a -> Action ()
lucid h = do
    setHeader "Content-Type" "text/html"
    raw (renderBS h)

showPage :: Html () -> Action ()
showPage p = do
    cy <- liftIO currentYear
    lucid $ Pages.template cy p

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
        conf <- lift $ asks Env.config
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
        redirect $ "/admin/edit/" <> (DTL.fromStrict . toText . PostStorage.postId $ r)
    get    "/admin/edit/:postId" $ do
        Auth.require
        i <- pathParam "postId"
        mp <- withConn $ \conn -> PostView.get conn i
        case mp of
            Just p -> showPage $ Pages.editPost p
            Nothing -> do
                status NT.notFound404
                showPage $ Pages.notFound "Unknown post id"
    put    "/admin/edit/:postId" $ do
        Auth.require
        i <- pathParam "postId"
        t <- formParam "title"
        c <- formParam "content"
        d <- formParam "draft"
        case DA.eitherDecode c of
            Right bs -> do
                withConn $ \conn -> PostView.update conn i t bs d
                redirect $ toLazyText ("/admin/edit/" <> fromId i)
            Left m -> do
                status NT.badRequest400
                showPage $ Pages.badRequest (DT.pack m)
    delete "/admin/edit/:postId" $ do
        Auth.require
        pid <- pathParam "postId"
        withConn (`PostStorage.delete` pid)
        redirect "/admin"
    get    "/admin/preview/:postId" $ do
        Auth.require
        i <- pathParam "postId"
        mp <- withConn $ \conn -> PostView.get conn i
        case mp of
            Just p -> showPage $ Pages.preview p
            Nothing -> do
                status NT.notFound404
                showPage $ Pages.notFound "Unknown post id"

postData :: App ()
postData = do
    get "/data/images/:postId/:fileName" $ do
        Auth.require
        pid <- pathParam "postId"
        fn <- pathParam "fileName"
        mi <- withConn $ \conn -> Image.getByFileName conn pid fn
        case mi of
            Nothing -> status NT.notFound404
            Just img -> do
                setHeader "Content-Type" (DTL.fromStrict $ Image.imageMIMEType img)
                file (DT.unpack $ "data/images/" <> toText pid <> "/" <> fn)

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
        conf <- lift $ asks Env.config
        if Login.verify (admin conf) l p then do
            jwtEnv <- lift $ asks Env.jwt
            t <- liftIO $ JWT.issue jwtEnv
            json t
             else status NT.unauthorized401 >> jsonError "Bad credentials"
    post "/admin/api/token/renew" $ do
        mt <- header "Authroization"
        case mt of
            Nothing -> status NT.unauthorized401 >> jsonError "No authorization token"
            Just t -> do
                jwtEnv <- lift $ asks Env.jwt
                nt <- liftIO $ JWT.renew jwtEnv (DTL.toStrict t)
                json nt

imageAPI :: App ()
imageAPI = do
    get    "/admin/api/image/:imageId" $ do
        Auth.require
        iid <- pathParam "imageId"
        r <- withConn (`Image.get` iid)
        json r
    put    "/admin/api/image/:imageId" $ do
        Auth.require
        i <- pathParam "imageId"
        c <- formParam "caption"
        r <- withConn  $ \conn -> Image.updateCaption conn i c
        json r
    delete "/admin/api/image/:imageId" $ do
        Auth.require
        iid <- pathParam "imageId"
        withConn (`Image.delete` iid)

postAPI :: App ()
postAPI = do
    get    "/admin/api/post/:postId" $ do
        Auth.require
        pid <- pathParam "postId"
        r <- withConn $ \conn -> PostView.get conn pid
        json r
    put    "/admin/api/post/:postId" $ do
        Auth.require
        i <- pathParam "postId"
        t <- formParam "title"
        c <- formParam "content"
        d <- formParam "draft"
        case DA.eitherDecode c of
            Right bs -> withConn $ \conn -> PostView.update conn i t bs d
            Left m -> status NT.badRequest400 >> json m
    delete "/admin/api/post/:postId" $ do
        Auth.require
        pid <- pathParam "postId"
        withConn $ \conn -> PostStorage.delete conn pid
    post   "/admin/api/post/:postId/image" $ do
        Auth.require
        i <- pathParam "postId"
        fs <- files
        conf <- askConfig
        r <- withConn $ \conn -> mapM (Image.upload conn conf i) fs
        json r
