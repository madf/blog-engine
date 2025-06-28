module Madf.Blog
    ( serve
    , routes
    ) where

import Data.Text.Lazy.Builder
import qualified Data.Text as DT
import Data.Maybe
import Data.Pool
import qualified Data.Aeson as DA
import Database.SQLite.Simple
import Control.Monad.Reader
import Web.Scotty.Trans as WS
import qualified Network.HTTP.Types as NT
import Network.Wai.Middleware.Static
import qualified Madf.Blog.Post as Post
import qualified Madf.Blog.Env as Env
import Madf.Blog.Ids
import qualified Madf.Blog.Pages as Pages
import qualified Madf.Blog.API as API
import qualified Madf.Blog.DB as DB
import Lucid

type App a = ScottyT Env.EnvM a

askPool :: ActionT Env.EnvM (Pool Connection)
askPool = lift $ asks Env.pool

--askConfig :: App C.Config
--askConfig = lift $ asks config

serve :: IO ()
serve = do
    env <- Env.create "config.ini"
    withResource (Env.pool env) DB.check
    scottyOptsT WS.defaultOptions (Env.runIO env) routes

routes :: App ()
routes = do
    middleware $ staticPolicy (noDots >-> addBase "static")
    pages
    api

lucid :: Html a -> ActionT Env.EnvM ()
lucid h = do
    setHeader "Content-Type" "text/html"
    raw (renderBS h)

pages :: App ()
pages = do
    get    "/admin" $ do
        page <- queryParamMaybe "page"
        perPage <- queryParamMaybe "perPage"
        pool <- askPool
        posts <- liftIO . withResource pool $ \conn -> Post.list conn (fromMaybe 0 page) (fromMaybe 10 perPage)
        lucid (Pages.mainPage posts)
    get    "/admin/new" $ lucid Pages.newPost
    post   "/admin/new" $ do
        t <- formParam "title"
        c <- formParam "contents"
        case DA.eitherDecode c of
            Right bs -> do
                pool <- askPool
                liftIO . withResource pool $ \conn -> Post.create conn t bs
                redirect "/admin"
            Left m -> do
                status NT.badRequest400
                lucid $ Pages.badRequest (DT.pack m)
    get    "/admin/edit/:postId" $ do
        i <- pathParam "postId"
        pool <- askPool
        mp <- liftIO . withResource pool $ \conn -> Post.get conn i
        case mp of
            Just p -> lucid $ Pages.editPost p
            Nothing -> do
                status NT.notFound404
                lucid $ Pages.notFound "Unknown post id"
    put    "/admin/edit/:postId" $ do
        i <- pathParam "postId"
        t <- formParam "title"
        c <- formParam "contents"
        d <- formParam "draft"
        case DA.eitherDecode c of
            Right bs -> do
                pool <- askPool
                liftIO . withResource pool $ \conn -> Post.update conn i t bs d
                redirect $ toLazyText ("/admin/edit/" <> fromId i)
            Left m -> do
                status NT.badRequest400
                lucid $ Pages.badRequest (DT.pack m)
    delete "/admin/edit/:postId" $ do
        pid <- pathParam "postId"
        pool <- askPool
        liftIO (withResource pool (`Post.delete` pid)) >> redirect "/admin"
    get    "/admin/preview/:postId" $ do
        i <- pathParam "postId"
        pool <- askPool
        mp <- liftIO . withResource pool $ \conn -> Post.get conn i
        case mp of
            Just p -> lucid $ Pages.previewPost p
            Nothing -> do
                status NT.notFound404
                lucid $ Pages.notFound "Unknown post id"

api :: App ()
api = do
    get    "/admin/api/image/:imageId" $ do
        iid <- pathParam "imageId"
        pool <- askPool
        liftIO (withResource pool (`API.getImageInfo` iid)) >>= json
    put    "/admin/api/image/:imageId" $ do
        i <- pathParam "imageId"
        c <- formParam "caption"
        pool <- askPool
        liftIO $ withResource pool (\conn -> API.updateImageInfo conn i c)
    delete "/admin/api/image/:imageId" $ do
        iid <- pathParam "imageId"
        pool <- askPool
        liftIO (withResource pool (`API.deleteImageInfo` iid))
    post   "/admin/api/post" $ do
        pool <- askPool
        liftIO (withResource pool API.newPost) >>= json
    get    "/admin/api/post/:postId" $ do
        pid <- pathParam "postId"
        pool <- askPool
        liftIO (withResource pool $ \conn -> API.getPostInfo conn pid) >>= json
    put    "/admin/api/post/:postId" $ do
        i <- pathParam "postId"
        cap <- formParam "caption"
        cont <- formParam "contents"
        pool <- askPool
        liftIO $ withResource pool (\conn -> API.updatePostInfo conn i cap cont)
    delete "/admin/api/post/:postId" $ do
        pid <- pathParam "postId"
        pool <- askPool
        liftIO (withResource pool $ \conn -> API.deletePostInfo conn pid)
    post   "/admin/api/post/:postId/image" $ do
        i <- pathParam "postId"
        fs <- files
        pool <- askPool
        r <- liftIO $ withResource pool (\conn -> API.uploadImage conn i fs)
        json r
