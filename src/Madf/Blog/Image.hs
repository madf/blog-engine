module Madf.Blog.Image
    ( Image (..)
    , create
    , get
    , updateFile
    , updateCaption
    , delete
    , listByPost
    , deleteByPost
    , listOrphaned
    , previewUrl
    , imageUrl
    ) where

import Data.Text
import Data.Time
import Data.ByteString
import Data.Int
import Data.Aeson
import Database.SQLite.Simple
import Madf.Blog.Ids

data Image = Image
    { imageId              :: !ImageId
    , imagePostId          :: !(Maybe Int64)
    , imageCaption         :: !Text
    , imageFileName        :: !Text
    , imageFileSize        :: !Int64
    , imageWidth           :: !Int
    , imageHeight          :: !Int
    , imageMIMEType        :: !Text
    , imagePreviewFileName :: !Text
    , imagePreviewFileSize :: !Int64
    , imagePreviewWidth    :: !Int
    , imagePreviewHeight   :: !Int
    , imageCreated         :: !UTCTime
    , imageUpdated         :: !UTCTime
    } deriving (Show)

instance ToJSON Image
    where
        toJSON v = object
            [ "id"                .= imageId v
            , "post_id"           .= imagePostId v
            , "caption"           .= imageCaption v
            , "file_name"         .= imageFileName v
            , "file_size"         .= imageFileSize v
            , "width"             .= imageWidth v
            , "height"            .= imageHeight v
            , "mime_type"         .= imageMIMEType v
            , "preview_file_name" .= imagePreviewFileName v
            , "preview_file_size" .= imagePreviewFileSize v
            , "preview_width"     .= imagePreviewWidth v
            , "preview_height"    .= imagePreviewHeight v
            , "created"           .= imageCreated v
            , "updated"           .= imageUpdated v
            ]
        toEncoding v = pairs
            (  "id"                .= imageId v
            <> "post_id"           .= imagePostId v
            <> "caption"           .= imageCaption v
            <> "file_name"         .= imageFileName v
            <> "file_size"         .= imageFileSize v
            <> "width"             .= imageWidth v
            <> "height"            .= imageHeight v
            <> "mime_type"         .= imageMIMEType v
            <> "preview_file_name" .= imagePreviewFileName v
            <> "preview_file_size" .= imagePreviewFileSize v
            <> "preview_width"     .= imagePreviewWidth v
            <> "preview_height"    .= imagePreviewHeight v
            <> "created"           .= imageCreated v
            <> "updated"           .= imageUpdated v
            )

instance FromJSON Image
    where
        parseJSON = withObject "Madf.Blog.Image" $ \o -> Image
            <$> o .: "id"
            <*> o .: "post_id"
            <*> o .: "caption"
            <*> o .: "file_name"
            <*> o .: "file_size"
            <*> o .: "width"
            <*> o .: "height"
            <*> o .: "mime_type"
            <*> o .: "preview_file_name"
            <*> o .: "preview_file_size"
            <*> o .: "preview_width"
            <*> o .: "preview_height"
            <*> o .: "created"
            <*> o .: "updated"

create :: Connection -> ByteString -> IO Image
create = undefined

get :: Connection -> ImageId -> IO (Maybe Image)
get = undefined

updateFile :: Connection -> ImageId -> ByteString -> IO Image
updateFile = undefined

updateCaption :: Connection -> ImageId -> Text -> IO Image
updateCaption = undefined

delete :: Connection -> ImageId -> IO ()
delete = undefined

listByPost :: Connection -> PostId -> IO [Image]
listByPost = undefined

deleteByPost :: Connection -> PostId -> IO ()
deleteByPost = undefined

listOrphaned :: Connection -> IO [Image]
listOrphaned = undefined

imageUrlPrefix :: Image -> Text
imageUrlPrefix i = "/blogimages/" <> (Data.Text.pack . show) (imagePostId i)

previewUrl :: Image -> Text
previewUrl i = imageUrlPrefix i <> "/" <> imagePreviewFileName i

imageUrl :: Image -> Text
imageUrl i = imageUrlPrefix i <> "/" <> imageFileName i
