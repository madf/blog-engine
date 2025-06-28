{-# LANGUAGE DeriveGeneric  #-}
{-# LANGUAGE DeriveAnyClass #-}

module Madf.Blog.Image
    ( Image (..)
    , get
    , updateFile
    , updateCaption
    , delete
    , listByPost
    , deleteByPost
    , previewUrl
    , imageUrl
    , imagePreviewUrl
    , upload
    ) where

import GHC.Generics
import Control.Monad
import Data.Text
import Data.Text.Encoding
import Data.Time
import qualified Data.ByteString.Lazy as BS
import Data.Int
import Data.Maybe
import Data.Aeson
import Database.SQLite.Simple
import Web.Scotty (File)
import Network.Wai.Parse (FileInfo (..))
import qualified Codec.Picture as CP
import qualified Codec.Picture.Metadata as CP
import qualified Codec.Picture.Extra as CPE
import Madf.Blog.Ids
import Madf.Blog.Config
import Madf.Blog.Files

data Image = Image
    { imageId              :: !ImageId
    , imagePostId          :: !PostId
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
    , imageUpdated         :: !(Maybe UTCTime)
    } deriving (Show, Generic, FromRow)

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

data Info = Info !PostId !Text !Int64 !Int !Int !Text !Text !Int64 !Int !Int !UTCTime deriving (Show, Generic, ToRow)

fromInfo :: ImageId -> Info -> Image
fromInfo iid (Info pid fn fs w h mime pfn pfs pw ph c) = Image iid pid "" fn fs w h mime pfn pfs pw ph c Nothing

get :: Connection -> ImageId -> IO (Maybe Image)
get conn iid = listToMaybe <$> query conn "SELECT id, post_id caption, file_name, file_size, width, height, mime_type, preview_file_name, preview_file_size, preview_width, preview_height, created, updated FROM images WHERE id = ?" (Only iid)

updateFile :: Connection -> ImageId -> File BS.ByteString -> IO Image
updateFile = undefined

updateCaption :: Connection -> ImageId -> Text -> IO Image
updateCaption = undefined

delete :: Connection -> ImageId -> IO ()
delete conn iid = do
    mfs <- listToMaybe <$> query conn "SELECT file_name, preview_file_name FROM images WHERE id = ?" (Only iid)
    case mfs of
        Nothing -> return ()
        Just (sfn, spn) -> do
            removeFiles sfn spn
            void $ execute conn "DELETE FROM images WHERE id = ?" (Only iid)

listByPost :: Connection -> PostId -> IO [Image]
listByPost conn pid = query conn "SELECT id, post_id caption, file_name, file_size, width, height, mime_type, preview_file_name, preview_file_size, preview_width, preview_height, created, updated FROM images WHERE post_id = ?" (Only pid)

deleteByPost :: Connection -> PostId -> IO ()
deleteByPost conn pid = do
    fs <- query conn "SELECT file_name, preview_file_name FROM images WHERE post_id = ?" (Only pid)
    mapM_ (\(sfn, spn) -> removeFiles sfn spn) fs
    void $ execute conn "DELETE FROM images WHERE post_id = ?" (Only pid)

imageUrlPrefix :: Image -> Text
imageUrlPrefix i = "/blogimages/" <> (Data.Text.pack . show) (imagePostId i)

previewUrl :: Image -> Text
previewUrl i = imageUrlPrefix i <> "/" <> imagePreviewFileName i

imageUrl :: Image -> Text
imageUrl i = imageUrlPrefix i <> "/" <> imageFileName i

imagePreviewUrl :: Image -> Text
imagePreviewUrl i = imageUrlPrefix i <> "/" <> imagePreviewFileName i

scaleToHeight :: Int -> CP.Image CP.PixelRGBA8 -> CP.Image CP.PixelRGBA8
scaleToHeight dh img = CPE.scaleBilinear (w * dh `div` h) dh img
    where
        w = CP.imageWidth img
        h = CP.imageHeight img

createPreview :: Int -> Int -> Text -> CP.Metadatas -> CP.DynamicImage -> IO (Int, Int)
createPreview jq dh pfp md img = do
    let simg = scaleToHeight dh (CP.convertRGBA8 img)
    save $ CP.ImageRGBA8 simg
    return (CP.imageWidth simg, CP.imageHeight simg)
    where
        save simg = case CP.lookup CP.Format md of
            Just CP.SourceJpeg -> CP.saveJpgImage jq (unpack pfp) simg
            Just CP.SourcePng  -> CP.savePngImage (unpack pfp) simg
            v                  -> error $ "Unsupported image format: '" ++ show v ++ "'"

upload :: Connection -> Config -> PostId -> File BS.ByteString -> IO Image
upload conn conf pid (_, fi) = do
    pe <- checkPId
    unless pe (error "Unknown post id")
    checkCreateDir stp
    (md, img) <- prepareImage sfn fi
    (pw, ph) <- createPreview jq dph spn md img
    fs <- getSize sfn
    ps <- getSize spn
    now <- getCurrentTime
    ri <- createImage conn (Info pid fn fs (width img) (height img) mime pn ps pw ph now)
    case ri of
        Left e -> cleanup e
        Right i -> return i
    where
        checkPId = or . fmap fromOnly <$> query conn "SELECT EXISTS (SELECT 1 FROM posts WHERE id = ?)" (Only pid)
        fn = decodeUtf8 $ fileName fi
        pn = previewPrefix (images conf) <> fn
        std = storageDir $ images conf
        stp = std <> toText pid
        sfn = stp <> "/" <> fn
        spn = stp <> "/" <> pn
        width = CP.dynamicMap CP.imageWidth
        height = CP.dynamicMap CP.imageHeight
        dph = previewHeight $ images conf
        jq = jpegQuality $ images conf
        mime = decodeUtf8 $ fileContentType fi
        cleanup m = do
            removeFiles sfn spn
            error (unpack m)

createImage :: Connection -> Info -> IO (Either Text Image)
createImage conn info = do
    miid <- fmap fromOnly . listToMaybe <$> query conn "INSERT INTO images (post_id, caption, file_name, file_size, width, height, mime_type, preview_file_name, preview_file_size, preview_width, preview_height, created, updated) VALUES (?, '', ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, NULL) RETURNING id" info :: IO (Maybe ImageId)
    case miid of
        Nothing -> return $ Left "Cannot insert image into the DB"
        Just iid -> return $ Right (fromInfo iid info)

prepareImage :: Text -> FileInfo BS.ByteString -> IO (CP.Metadatas, CP.DynamicImage)
prepareImage fn fi = do
    BS.writeFile (unpack fn) cnt
    r <- CP.readImageWithMetadata (unpack fn)
    case r of
        Left e -> error e
        Right (img, md) -> return (md, img)
    where
        cnt = fileContent fi

removeFiles :: Text -> Text -> IO ()
removeFiles sfn spn = do
    removeIfExists sfn
    removeIfExists spn
