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
import Control.Monad
import Control.Monad.Reader
import Web.Scotty.Trans as WS
import qualified Network.HTTP.Types as NT
import Network.Wai.Middleware.Static
import qualified Madf.Blog.Post as Post
import qualified Madf.Blog.Image as Image
import qualified Madf.Blog.Env as Env
import Madf.Blog.Config
import Madf.Blog.Ids
import qualified Madf.Blog.Pages as Pages
import qualified Madf.Blog.DB as DB
import Lucid

type App a = ScottyT Env.EnvM a

askPool :: ActionT Env.EnvM (Pool Connection)
askPool = lift $ asks Env.pool

withConn :: (Connection -> IO a) -> ActionT Env.EnvM a
withConn f = do
    pool <- askPool
    liftIO $ withResource pool f

askConfig :: ActionT Env.EnvM Config
askConfig = lift $ asks Env.config

serve :: Env.Env -> IO ()
serve env = do
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
        posts <- withConn $ \conn -> Post.list conn (fromMaybe 0 page) (fromMaybe 10 perPage)
        lucid (Pages.mainPage posts)
    get    "/admin/new" $ lucid Pages.newPost
    post   "/admin/new" $ do
        t <- formParam "title"
        c <- formParam "content"
        case DA.eitherDecode c of
            Right bs -> do
                withConn $ \conn -> void $ Post.create conn t bs
                redirect "/admin"
            Left m -> do
                status NT.badRequest400
                lucid $ Pages.badRequest (DT.pack m)
    get    "/admin/edit/:postId" $ do
        i <- pathParam "postId"
        mp <- withConn $ \conn -> Post.get conn i
        case mp of
            Just p -> lucid $ Pages.editPost p
            Nothing -> do
                status NT.notFound404
                lucid $ Pages.notFound "Unknown post id"
    put    "/admin/edit/:postId" $ do
        i <- pathParam "postId"
        t <- formParam "title"
        c <- formParam "content"
        d <- formParam "draft"
        case DA.eitherDecode c of
            Right bs -> do
                withConn $ \conn -> Post.update conn i t bs d
                redirect $ toLazyText ("/admin/edit/" <> fromId i)
            Left m -> do
                status NT.badRequest400
                lucid $ Pages.badRequest (DT.pack m)
    delete "/admin/edit/:postId" $ do
        pid <- pathParam "postId"
        withConn (`Post.delete` pid)
        redirect "/admin"
    get    "/admin/preview/:postId" $ do
        i <- pathParam "postId"
        mp <- withConn $ \conn -> Post.get conn i
        case mp of
            Just p -> lucid $ Pages.previewPost p
            Nothing -> do
                status NT.notFound404
                lucid $ Pages.notFound "Unknown post id"

api :: App ()
api = do
    imageAPI
    postAPI

imageAPI :: App ()
imageAPI = do
    get    "/admin/api/image/:imageId" $ do
        iid <- pathParam "imageId"
        r <- withConn (`Image.get` iid)
        json r
    put    "/admin/api/image/:imageId" $ do
        i <- pathParam "imageId"
        c <- formParam "caption"
        r <- withConn  $ \conn -> Image.updateCaption conn i c
        json r
    delete "/admin/api/image/:imageId" $ do
        iid <- pathParam "imageId"
        withConn (`Image.delete` iid)

postAPI :: App ()
postAPI = do
    post   "/admin/api/post" $ do
        t <- formParam "title"
        c <- formParam "content"
        case DA.eitherDecode c of
            Right bs -> do
                r <- withConn $ \conn -> Post.create conn t bs
                json r
            Left m -> status NT.badRequest400 >> json m
    get    "/admin/api/post/:postId" $ do
        pid <- pathParam "postId"
        r <- withConn $ \conn -> Post.get conn pid
        json r
    put    "/admin/api/post/:postId" $ do
        i <- pathParam "postId"
        t <- formParam "title"
        c <- formParam "content"
        d <- formParam "draft"
        case DA.eitherDecode c of
            Right bs -> withConn $ \conn -> Post.update conn i t bs d
            Left m -> status NT.badRequest400 >> json m
    delete "/admin/api/post/:postId" $ do
        pid <- pathParam "postId"
        withConn $ \conn -> Post.delete conn pid
    post   "/admin/api/post/:postId/image" $ do
        i <- pathParam "postId"
        fs <- files
        conf <- askConfig
        r <- withConn $ \conn -> mapM (Image.upload conn conf i) fs
        json r
