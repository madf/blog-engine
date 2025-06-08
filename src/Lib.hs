module Lib
    ( serve
    , routes
    ) where

import Data.Text.Lazy
import Data.Text.Lazy.Builder
import Data.Maybe
import Database.SQLite.Simple
import Web.Scotty
import Madf.Blog
import qualified Madf.Blog.Post as Post
import Madf.Blog.Ids
import Madf.Blog.Utils

serve :: IO ()
serve = do
    conn <- open "test.db"
    scotty 3000 (routes conn)

routes :: Connection -> ScottyM ()
routes conn = do
    get    "/admin" $ do
        page <- queryParamMaybe "page"
        perPage <- queryParamMaybe "perPage"
        posts <- liftIO $ Post.list conn (fromMaybe 0 page) (fromMaybe 10 perPage)
        lucid (mainPage posts)
    get    "/admin/new" $ lucid newPost
    post   "/admin/new" $ do
        t <- formParam "title"
        c <- formParam "contents"
        liftIO $ Post.create conn t c
        redirect "/admin"
    get    "/admin/edit/:postId" $ do
        i <- pathParam "postId"
        lucid (editPost conn i)
    put    "/admin/edit/:postId" $ do
        i <- pathParam "postId"
        t <- formParam "title"
        c <- formParam "contents"
        d <- formParam "draft"
        liftIO $ Post.update conn i (Post.Post i t c d)
        redirect $ "/admin/edit/" <> fromStrict i
    delete "/admin/edit/:postId" $ pathParam "postId" >>= liftIO .Post.delete conn >> redirect "/admin"
    post   "/admin/api/image" $ files >>= liftIO (uploadImage conn) >>= json
    get    "/admin/api/image/:imageId" $ pathParam "imageId" >>= liftIO . getImageInfo conn >>= json
    put    "/admin/api/image/:imageId" $ do
        i <- pathParam "imageId"
        c <- formParam "caption"
        liftIO $ updateImageInfo conn i c
    delete "/admin/api/image/:imageId" $ pathParam "imageId" >>= liftIO . deleteImageInfo conn
