module Madf.Blog.API
    ( uploadImage
    , getImageInfo
    , updateImageInfo
    , deleteImageInfo
    ) where

import Data.Text
import qualified Data.ByteString.Lazy as BS
import Database.SQLite.Simple
import Web.Scotty
import qualified Madf.Blog.Image as Image
import Madf.Blog.Ids

uploadImage :: Connection -> [File BS.ByteString] -> IO ImageId
uploadImage = undefined

getImageInfo :: Connection -> ImageId -> IO (Maybe Image.Image)
getImageInfo = undefined

updateImageInfo :: Connection -> ImageId -> Text -> IO ()
updateImageInfo = undefined

deleteImageInfo :: Connection -> ImageId -> IO ()
deleteImageInfo = undefined
