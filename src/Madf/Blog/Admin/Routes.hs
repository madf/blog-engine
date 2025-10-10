module Madf.Blog.Admin.Routes
    ( routes
    ) where

import Control.Monad
import Control.Monad.Reader
import Data.Text qualified as DT
import Data.Text.Lazy qualified as DTL
import Data.Text.Encoding
import Data.Maybe
import Data.Aeson qualified as DA
import Web.Scotty.Trans
import Web.Scotty.Cookie
import Network.HTTP.Types qualified as NT
import Madf.Blog.App
import Madf.Blog.Config qualified as Config
import Madf.Blog.Slug qualified as Slug
import Madf.Blog.Image qualified as Image
import Madf.Blog.Post.Storage qualified as PostStorage
import Madf.Blog.Post.View qualified as PostView
import Madf.Blog.Admin.Pages.Edit qualified as Pages
import Madf.Blog.Admin.Pages.Preview qualified as Pages
import Madf.Blog.Admin.Pages.Login qualified as Pages
import Madf.Blog.Admin.Pages.Template qualified as Pages
import Madf.Blog.Admin.Pages.Index qualified as Pages
import Madf.Blog.Admin.Pages.NotFound qualified as Pages
import Madf.Blog.Admin.Publish
import Madf.Blog.Admin.Auth qualified as Auth
import Madf.Blog.Admin.Login qualified as Login
import Madf.Blog.Env qualified as Env
import Madf.Blog.JWT qualified as JWT
import Madf.Blog.Time
import Madf.Blog.ToText
import Madf.Blog.Contents qualified as Contents
import Lucid

routes :: App ()
routes = do
    pages
    api

pages :: App ()
pages = do
    get  "/admin" getAdminIndexPage
    get  "/admin/login" getAdminLoginPage
    post "/admin/login" handleAdminLogin
    post "/admin/posts/new" createNewPost
    get  "/admin/posts/:postSlug" getPostPreviewPage
    get  "/admin/posts/:postSlug/edit" getPostEditPage
    get  "/admin/years/:year" getYearPostsPage

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
        if Login.verify (Config.admin conf) l p then do
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
    post   "/admin/api/post/regenerate" $ do
        Auth.requireNoRedirect
        conf <- askConfig
        withConn $ \conn -> liftIO $ regenerateAll conn conf

lucid :: Html a -> Action ()
lucid = html . renderText

showPage :: ([(DT.Text, DT.Text)], DT.Text) -> Html () -> Action ()
showPage breadcrumbs p = do
    cy <- liftIO currentYear
    cnt <- withConn $ \conn -> Contents.get conn cy 0 maxBound
    lucid $ Pages.template breadcrumbs cy cnt p

jsonError :: DT.Text -> Action ()
jsonError = json

getAdminIndexPage :: Action ()
getAdminIndexPage = do
    Auth.require
    page <- queryParamMaybe "page"
    perPage <- queryParamMaybe "perPage"
    posts <- withConn $ \conn -> PostView.list conn (fromMaybe 0 page) (fromMaybe 10 perPage)
    showPage ([], "Home") $ Pages.index posts

getAdminLoginPage :: Action ()
getAdminLoginPage = do
    me <- queryParamMaybe "error"
    cy <- liftIO currentYear
    lucid $ Pages.login cy me

handleAdminLogin :: Action ()
handleAdminLogin = do
    f <- queryParamMaybe "from"
    l <- formParam "login"
    p <- formParam "password"
    conf <- askConfig
    if Login.verify (Config.admin conf) l p
        then do
            jwtEnv <- lift $ asks Env.jwt
            t <- liftIO $ JWT.issue jwtEnv
            setCookie $ defaultSetCookie { setCookieName = "authtoken", setCookieValue = encodeUtf8 t, setCookiePath = Just "/" }
            redirect (fromMaybe "/admin" f)
        else Auth.redirectUnauthorized (DTL.toStrict <$> f) "Bad credentials"

createNewPost :: Action ()
createNewPost = do
    Auth.require
    r <- withConn $ \conn -> PostStorage.create conn
    redirect $ "/admin/edit/" <> (DTL.fromStrict . Slug.unSlug . PostStorage.postSlug $ r)

getPostEditPage :: Action ()
getPostEditPage = do
    Auth.require
    i <- pathParam "postSlug"
    mp <- withConn $ \conn -> PostView.get conn i
    case mp of
        Just p -> do
            let title = if PostView.postTitle p == "" then "Untitled" else PostView.postTitle p
                breadcrumbs = ([("Home", "/admin")], title)
            showPage breadcrumbs $ Pages.edit p
        Nothing -> do
            status NT.notFound404
            showPage ([], "Not Found") $ Pages.notFound "Unknown post id"

getPostPreviewPage :: Action ()
getPostPreviewPage = do
    Auth.require
    i <- pathParam "postSlug"
    mp <- withConn $ \conn -> PostView.get conn i
    case mp of
        Just p -> do
            let title = if PostView.postTitle p == "" then "Untitled" else PostView.postTitle p
                breadcrumbs = ([("Home", "/admin")], title)
            showPage breadcrumbs $ Pages.preview False p
        Nothing -> do
            status NT.notFound404
            showPage ([], "Not Found") $ Pages.notFound "Unknown post id"

getYearPostsPage :: Action ()
getYearPostsPage = do
    Auth.require
    y <- pathParam "year"
    page <- queryParamMaybe "page"
    perPage <- queryParamMaybe "perPage"
    posts <- withConn $ \conn -> PostView.year conn y (fromMaybe 0 page) (fromMaybe 10 perPage)
    showPage ([("Home", "/admin")], toText y) $ Pages.index posts
