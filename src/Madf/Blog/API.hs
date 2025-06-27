module Madf.Blog.API
    ( uploadImage
    , getImageInfo
    , updateImageInfo
    , deleteImageInfo
    , newPost
    , getPostInfo
    , updatePostInfo
    , deletePostInfo
    ) where

import Data.Text
import qualified Data.ByteString.Lazy as BS
import Database.SQLite.Simple
import Web.Scotty
import qualified Madf.Blog.Image as Image
import qualified Madf.Blog.Post as Post
import Madf.Blog.Ids

uploadImage :: Connection -> PostId -> [File BS.ByteString] -> IO ImageId
uploadImage = undefined

getImageInfo :: Connection -> ImageId -> IO (Maybe Image.Image)
getImageInfo = undefined

updateImageInfo :: Connection -> ImageId -> Text -> IO ()
updateImageInfo = undefined

deleteImageInfo :: Connection -> ImageId -> IO ()
deleteImageInfo = undefined

newPost :: Connection -> IO Post.Post
newPost = undefined

getPostInfo :: Connection -> PostId -> IO Post.Post
getPostInfo = undefined

updatePostInfo :: Connection -> PostId -> Text -> Text -> IO ()
updatePostInfo = undefined

deletePostInfo :: Connection -> PostId -> IO ()
deletePostInfo = undefined
