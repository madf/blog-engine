module Madf.Blog.Admin.Routes
    ( routes
    ) where

import Control.Monad
import Control.Monad.Reader
import Control.Monad.IO.Unlift
import Data.Text qualified as DT
import Data.Text.Encoding
import Data.Time.Clock
import Data.Maybe
import Data.Either
import Data.Pool (withResource)
import Data.Aeson qualified as DA
import Database.SQLite.Simple (Connection)
import Web.Scotty.Trans
import Web.Scotty.Cookie
import Network.HTTP.Types qualified as NT
import Madf.Blog.App
import Madf.Blog.Config qualified as Config
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
import Madf.Blog.Job qualified as Job
import Madf.Blog.Job (JobConcurrency(..))
import Madf.Blog.Time
import Madf.Blog.ToText
import Madf.Blog.Contents qualified as Contents
import Lucid
import UnliftIO.Exception

routes :: App ()
routes = do
    pages
    api

pages :: App ()
pages = do
    get  "/admin" getAdminIndexPage
    get  "/admin/login" getAdminLoginPage
    get  "/admin/posts/:postSlug" getPostPreviewPage
    get  "/admin/posts/:postSlug/edit" getPostEditPage
    get  "/admin/years/:year" getYearPostsPage

api :: App ()
api = do
    loginAPI
    imageAPI
    postAPI
    jobAPI

loginAPI :: App ()
loginAPI = do
    post "/admin/api/token/issue" $ do
        l <- formParam "login"
        p <- formParam "password"
        conf <- asks Env.config
        if Login.verify (Config.admin conf) l p
            then do
                jwtEnv <- lift $ asks Env.jwt
                t <- liftIO $ JWT.issue jwtEnv
                setAuthCookie t
                json t
            else status NT.unauthorized401 >> jsonError "Bad credentials"
    post "/admin/api/token/renew" $ do
        Auth.requireHeader
        jwtEnv <- lift $ asks Env.jwt
        nt <- liftIO $ JWT.issue jwtEnv
        setAuthCookie nt
        json nt

imageAPI :: App ()
imageAPI = do
    get    "/admin/api/images/:imageId" $ do
        Auth.requireHeader
        iid <- pathParam "imageId"
        r <- withConn (`Image.get` iid)
        json r
    delete "/admin/api/images/:imageId" $ do
        Auth.requireHeader
        iid <- pathParam "imageId"
        withConn (`Image.delete` iid)
    post   "/admin/api/images/regeneratePreviews" $ do
        Auth.requireHeader
        jobEnv <- asks Env.job
        conf <- asks Env.config
        pool <- asks Env.pool
        mjid <- lift $ Job.enqueue jobEnv "Images preview regeneration" (Exclusive "preview-regen") $ \pCb -> do
            withRunInIO $ \r -> do
                withResource pool $ \conn -> do
                    regeneratePreviews conn conf (r . pCb)
        case mjid of
            Just jid -> json jid
            Nothing -> status NT.conflict409 >> json ("Preview regeneration already in progress" :: DT.Text)

postAPI :: App ()
postAPI = do
    get    "/admin/api/posts" getAllPosts
    post   "/admin/api/posts" $ do
        Auth.requireHeader
        r <- withConn $ \conn -> PostStorage.create conn
        json r
    get    "/admin/api/posts/:postSlug" $ do
        Auth.requireHeader
        slug <- pathParam "postSlug"
        r <- withConn $ \conn -> PostView.get conn slug
        json r
    put    "/admin/api/posts/:postSlug" $ do
        Auth.requireHeader
        s <- pathParam "postSlug"
        t <- formParam "title"
        c <- formParam "content"
        ty <- formParam "type"
        r <- formParam "reason"
        d <- formParam "draft"
        conf <- asks Env.config
        case DA.eitherDecode c of
            Right bs -> withConn $ \conn -> do
                PostView.update conn s t bs (PostStorage.makeType ty r) d
                unless d (liftIO $ publish conn conf s)
            Left m -> status NT.badRequest400 >> json m
    post   "/admin/api/posts/:postSlug/image" $ do
        Auth.requireHeader
        i <- pathParam "postSlug"
        fs <- files
        conf <- asks Env.config
        r <- withConn $ \conn -> mapM (Image.upload conn conf i) fs
        json r
    post   "/admin/api/posts/regenerate" $ do
        Auth.requireHeader
        conf <- asks Env.config
        withConn $ \conn -> liftIO $ regenerateAll conn conf

jobAPI :: App ()
jobAPI = do
    get    "/admin/api/jobs" getAllJobs
    get    "/admin/api/jobs/:jobId" getJob
    delete "/admin/api/jobs/:jobId" deleteJob

lucid :: Html a -> Action ()
lucid = html . renderText

showPage :: ([(DT.Text, DT.Text)], DT.Text) -> Html () -> Action ()
showPage = showPageForYear Nothing

showPageForYear :: Maybe Year -> ([(DT.Text, DT.Text)], DT.Text) -> Html () -> Action ()
showPageForYear mYear breadcrumbs p = do
    cy <- liftIO currentYear
    let selectedYear = fromMaybe cy mYear
    cnt <- withConn $ \conn -> Contents.get conn selectedYear 0 maxBound
    lucid $ Pages.template breadcrumbs cy cnt p

jsonError :: DT.Text -> Action ()
jsonError = json

setAuthCookie :: DT.Text -> Action ()
setAuthCookie token =
    setCookie $ defaultSetCookie
        { setCookieName = "authtoken"
        , setCookieValue = encodeUtf8 token
        , setCookiePath = Just "/"
        , setCookieSecure = True
        , setCookieSameSite = Just sameSiteLax
        -- Note: NOT HttpOnly - JavaScript needs to read this cookie
        -- to send Authorization header for API calls. This is safe because:
        -- 1. Cookie used for page navigation (automatic by browser)
        -- 2. JavaScript reads cookie and sends as Authorization header for API
        -- 3. API is CSRF-immune because cross-origin requests cannot add Authorization header
        }

getAdminIndexPage :: Action ()
getAdminIndexPage = do
    Auth.requireCookie
    page <- queryParamMaybe "page"
    perPage <- queryParamMaybe "perPage"
    posts <- withConn $ \conn -> PostView.list conn (fromMaybe 0 page) (fromMaybe 10 perPage)
    showPage ([], "Home") $ Pages.index posts

getAdminLoginPage :: Action ()
getAdminLoginPage = do
    me <- queryParamMaybe "error"
    cy <- liftIO currentYear
    lucid $ Pages.login cy me

getPostEditPage :: Action ()
getPostEditPage = do
    Auth.requireCookie
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
    Auth.requireCookie
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
    Auth.requireCookie
    y <- pathParam "year"
    page <- queryParamMaybe "page"
    perPage <- queryParamMaybe "perPage"
    posts <- withConn $ \conn -> PostView.year conn y (fromMaybe 0 page) (fromMaybe 10 perPage)
    showPageForYear (Just y) ([("Home", "/admin")], toText y) $ Pages.index posts

getAllPosts :: Action ()
getAllPosts = do
    Auth.requireHeader
    page <- fromMaybe 0 <$> queryParamMaybe "page"
    perPage <- fromMaybe 10 <$> queryParamMaybe "perPage"
    ps <- withConn $ \conn -> PostView.list conn page perPage
    json ps

getAllJobs :: Action ()
getAllJobs = do
    Auth.requireHeader
    jobEnv <- lift $ asks Env.job
    jobs <- Job.list jobEnv
    json jobs

getJob :: Action ()
getJob = do
    Auth.requireHeader
    jid <- pathParam "jobId"
    jobEnv <- lift $ asks Env.job
    job <- Job.getStatus jobEnv jid
    maybe (status NT.notFound404 >> json ()) json job

deleteJob :: Action ()
deleteJob = do
    Auth.requireHeader
    jid <- pathParam "jobId"
    jobEnv <- lift $ asks Env.job
    void $ Job.cancel jobEnv jid
    json ()

data RegenResult = RegenResult
    { numImages   :: !Int
    , numFailures :: !Int
    , failures    :: ![(DT.Text, DT.Text)]
    , duration    :: !NominalDiffTime
    } deriving (Show)

instance DA.ToJSON RegenResult
    where
        toJSON v = DA.object
            [ "num_images"   DA..= numImages v
            , "num_failures" DA..= numFailures v
            , "failures"     DA..= failures v
            , "duration"     DA..= duration v
            ]

regeneratePreviews :: Connection -> Config.Config -> (Int -> IO ()) -> IO DA.Value
regeneratePreviews conn conf pCb = do
    imgs <- Image.list conn
    let num = Prelude.length imgs
    if num == 0
        then return . DA.toJSON $ RegenResult 0 0 [] 0
        else do
            start <- getCurrentTime
            results <- forM (Prelude.zip imgs [0..]) $ \(img, imgNum) -> do
                r <- tryAny $ Image.regenPreview conn conf img
                pCb (imgNum * 100 `div` num)
                return (Image.imageFileName img, r)
            let errors = Prelude.map (\(fn, ex) -> (fn, DT.pack (show ex))) (Prelude.filter (isLeft . snd) results)
            end <- getCurrentTime
            return . DA.toJSON $ RegenResult num (Prelude.length errors) errors (diffUTCTime end start)
