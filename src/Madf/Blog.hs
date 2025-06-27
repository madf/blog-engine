module Madf.Blog
    ( serve
    , routes
    ) where

import Data.Text.Lazy.Builder
import qualified Data.Text as DT
import Data.Maybe
import qualified Data.Aeson as DA
import Database.SQLite.Simple
import Web.Scotty
import qualified Network.HTTP.Types as NT
import Network.Wai.Middleware.Static
import qualified Madf.Blog.Post as Post
import Madf.Blog.Ids
import Madf.Blog.Utils
import qualified Madf.Blog.Pages as Pages
import qualified Madf.Blog.API as API
import qualified Madf.Blog.DB as DB

serve :: IO ()
serve = do
    DB.check
    conn <- open "test.db"
    scotty 3000 (routes conn)

routes :: Connection -> ScottyM ()
routes conn = do
    middleware $ staticPolicy (noDots >-> addBase "static")
    pages conn
    api conn

pages :: Connection -> ScottyM ()
pages conn = do
    get    "/admin" $ do
        page <- queryParamMaybe "page"
        perPage <- queryParamMaybe "perPage"
        posts <- liftIO $ Post.list conn (fromMaybe 0 page) (fromMaybe 10 perPage)
        lucid (Pages.mainPage posts)
    get    "/admin/new" $ lucid Pages.newPost
    post   "/admin/new" $ do
        t <- formParam "title"
        c <- formParam "contents"
        case DA.eitherDecode c of
            Right bs -> do
                liftIO $ Post.create conn t bs
                redirect "/admin"
            Left m -> do
                status NT.badRequest400
                lucid $ Pages.badRequest (DT.pack m)
    get    "/admin/edit/:postId" $ do
        i <- pathParam "postId"
        mp <- liftIO $ Post.get conn i
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
                liftIO $ Post.update conn i t bs d
                redirect $ toLazyText ("/admin/edit/" <> fromId i)
            Left m -> do
                status NT.badRequest400
                lucid $ Pages.badRequest (DT.pack m)
    delete "/admin/edit/:postId" $ pathParam "postId" >>= liftIO .Post.delete conn >> redirect "/admin"
    get    "/admin/preview/:postId" $ do
        i <- pathParam "postId"
        mp <- liftIO $ Post.get conn i
        case mp of
            Just p -> lucid $ Pages.previewPost p
            Nothing -> do
                status NT.notFound404
                lucid $ Pages.notFound "Unknown post id"

api :: Connection -> ScottyM ()
api conn = do
    get    "/admin/api/image/:imageId" $ pathParam "imageId" >>= liftIO . API.getImageInfo conn >>= json
    put    "/admin/api/image/:imageId" $ do
        i <- pathParam "imageId"
        c <- formParam "caption"
        liftIO $ API.updateImageInfo conn i c
    delete "/admin/api/image/:imageId" $ pathParam "imageId" >>= liftIO . API.deleteImageInfo conn
    post   "/admin/api/post" $ liftIO (API.newPost conn) >>= json
    get    "/admin/api/post/:postId" $ pathParam "postId" >>= liftIO . API.getPostInfo conn >>= json
    put    "/admin/api/post/:postId" $ do
        i <- pathParam "postId"
        cap <- formParam "caption"
        cont <- formParam "contents"
        liftIO $ API.updatePostInfo conn i cap cont
    delete "/admin/api/post/:postId" $ pathParam "postId" >>= liftIO . API.deletePostInfo conn
    post   "/admin/api/post/:postId/image" $ do
        i <- pathParam "postId"
        fs <- files
        r <- liftIO $ API.uploadImage conn i fs
        json r
