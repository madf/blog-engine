{-# LANGUAGE DeriveGeneric  #-}
{-# LANGUAGE DeriveAnyClass #-}

module Madf.Blog.Image
    ( Image (..)
    , get
    , updateCaption
    , delete
    , getByFileName
    , imagePostId
    , imageCaption
    , imageURL
    , imageFileName
    , imageMIMEType
    , imagePreviewFileName
    , imagePreviewWidth
    , imagePreviewHeight
    , imagePreviewURL
    , upload
    , getMultiple
    ) where

import GHC.Generics
import Data.Text
import Data.Text.Encoding
import Data.Time
import Data.ByteString.Lazy qualified as BS
import Data.Int
import Data.Maybe
import Data.Aeson
import Database.SQLite.Simple
import Web.Scotty (File)
import Network.Wai.Parse (FileInfo (..))
import Codec.Picture qualified as CP
import Codec.Picture.Metadata qualified as CP
import Codec.Picture.Extra qualified as CPE
import Madf.Blog.Ids
import Madf.Blog.Config
import Madf.Blog.Files
import Madf.Blog.Time
import Madf.Blog.Utils
import Madf.Blog.Slug qualified as Slug
import Madf.Blog.ToText

data ImageInfo = ImageInfo
    { imageInfoPostId          :: !PostId
    , imageInfoCaption         :: !Text
    , imageInfoFileName        :: !Text
    , imageInfoFileSize        :: !Int64
    , imageInfoFileHash        :: !Int
    , imageInfoWidth           :: !Int
    , imageInfoHeight          :: !Int
    , imageInfoMIMEType        :: !Text
    , imageInfoURL             :: !Text
    , imageInfoPreviewFileName :: !Text
    , imageInfoPreviewFileSize :: !Int64
    , imageInfoPreviewWidth    :: !Int
    , imageInfoPreviewHeight   :: !Int
    , imageInfoPreviewURL      :: !Text
    , imageInfoCreated         :: !UTCTime
    , imageInfoUpdated         :: !(Maybe UTCTime)
    , imageInfoRefCount        :: !Int
    } deriving (Show, Generic, FromRow, ToRow)

instance FromJSON ImageInfo
    where
        parseJSON = withObject "Madf.Blog.ImageInfo" $ \o -> ImageInfo
            <$> o .: "post_id"
            <*> o .: "caption"
            <*> o .: "file_name"
            <*> o .: "file_size"
            <*> o .: "file_hash"
            <*> o .: "width"
            <*> o .: "height"
            <*> o .: "mime_type"
            <*> o .: "url"
            <*> o .: "preview_file_name"
            <*> o .: "preview_file_size"
            <*> o .: "preview_width"
            <*> o .: "preview_height"
            <*> o .: "preview_url"
            <*> o .: "created"
            <*> o .: "updated"
            <*> o .: "ref_count"

data Image = Image
    { imageId   :: !ImageId
    , imageInfo :: !ImageInfo
    } deriving (Show)

imagePostId :: Image -> PostId
imagePostId = imageInfoPostId . imageInfo

imageCaption :: Image -> Text
imageCaption = imageInfoCaption . imageInfo

imageURL :: Image -> Text
imageURL = imageInfoURL . imageInfo

imageFileName :: Image -> Text
imageFileName = imageInfoFileName . imageInfo

imageMIMEType :: Image -> Text
imageMIMEType = imageInfoMIMEType . imageInfo

imagePreviewFileName :: Image -> Text
imagePreviewFileName = imageInfoPreviewFileName . imageInfo

imagePreviewWidth :: Image -> Int
imagePreviewWidth = imageInfoPreviewWidth . imageInfo

imagePreviewHeight :: Image -> Int
imagePreviewHeight = imageInfoPreviewHeight . imageInfo

imagePreviewURL :: Image -> Text
imagePreviewURL = imageInfoPreviewURL . imageInfo

imageRefCount :: Image -> Int
imageRefCount = imageInfoRefCount . imageInfo

instance ToJSON Image
    where
        toJSON v = object
            [ "id"                .= imageId v
            , "post_id"           .= (imageInfoPostId . imageInfo) v
            , "caption"           .= (imageInfoCaption . imageInfo) v
            , "file_name"         .= (imageInfoFileName . imageInfo) v
            , "file_size"         .= (imageInfoFileSize . imageInfo) v
            , "file_hash"         .= (imageInfoFileHash . imageInfo) v
            , "width"             .= (imageInfoWidth . imageInfo) v
            , "height"            .= (imageInfoHeight . imageInfo) v
            , "mime_type"         .= (imageInfoMIMEType . imageInfo) v
            , "url"               .= (imageInfoURL . imageInfo) v
            , "preview_file_name" .= (imageInfoPreviewFileName . imageInfo) v
            , "preview_file_size" .= (imageInfoPreviewFileSize . imageInfo) v
            , "preview_width"     .= (imageInfoPreviewWidth . imageInfo) v
            , "preview_height"    .= (imageInfoPreviewHeight . imageInfo) v
            , "preview_url"       .= (imageInfoPreviewURL . imageInfo) v
            , "created"           .= (imageInfoCreated . imageInfo) v
            , "updated"           .= (imageInfoUpdated . imageInfo) v
            , "ref_count"         .= (imageInfoRefCount . imageInfo) v
            ]
        toEncoding v = pairs
            (  "id"                .= imageId v
            <> "post_id"           .= (imageInfoPostId . imageInfo) v
            <> "caption"           .= (imageInfoCaption . imageInfo) v
            <> "file_name"         .= (imageInfoFileName . imageInfo) v
            <> "file_size"         .= (imageInfoFileSize . imageInfo) v
            <> "file_hash"         .= (imageInfoFileHash . imageInfo) v
            <> "width"             .= (imageInfoWidth . imageInfo) v
            <> "height"            .= (imageInfoHeight . imageInfo) v
            <> "mime_type"         .= (imageInfoMIMEType . imageInfo) v
            <> "url"               .= (imageInfoURL . imageInfo) v
            <> "preview_file_name" .= (imageInfoPreviewFileName . imageInfo) v
            <> "preview_file_size" .= (imageInfoPreviewFileSize . imageInfo) v
            <> "preview_width"     .= (imageInfoPreviewWidth . imageInfo) v
            <> "preview_height"    .= (imageInfoPreviewHeight . imageInfo) v
            <> "preview_url"       .= (imageInfoPreviewURL . imageInfo) v
            <> "created"           .= (imageInfoCreated . imageInfo) v
            <> "updated"           .= (imageInfoUpdated . imageInfo) v
            <> "ref_count"         .= (imageInfoRefCount . imageInfo) v
            )

instance FromRow Image
    where
        fromRow = Image <$> field <*> fromRow

instance FromJSON Image
    where
        parseJSON j = (withObject "Madf.Blog.Image" $ \o -> Image
            <$> o .: "id"
            <*> parseJSON j)
            j

imageFields :: Query
imageFields = "images.id, images.post_id, images.caption, images.file_name, images.file_size, images.file_hash, images.width, images.height, images.mime_type, images.url, images.preview_file_name, images.preview_file_size, images.preview_width, images.preview_height, images.preview_url, images.created, images.updated, images.ref_count"

selectBase :: Query
selectBase = "SELECT " <> imageFields <> " FROM images"

get :: Connection -> ImageId -> IO (Maybe Image)
get conn iid = listToMaybe <$> query conn (selectBase <> " WHERE id = ?") (Only iid)

get' :: Connection -> ImageId -> IO Image
get' conn iid = do
    mr <- get conn iid
    case mr of
        Just img -> return img
        Nothing -> error "Unknown image id"

getMultiple :: Connection -> [ImageId] -> IO [Image]
getMultiple conn iids = withTransaction conn $ do
    execute_ conn "DROP TABLE IF EXISTS temp.image_ids"
    execute_ conn "CREATE TABLE temp.image_ids (id INTEGER NOT NULL)"
    mapM_ (execute conn "INSERT INTO temp.image_ids (id) VALUES (?)" . Only) iids
    r <- query_ conn ("SELECT " <> imageFields <> " FROM temp.image_ids LEFT JOIN images ON images.id = temp.image_ids.id")
    execute_ conn "DROP TABLE temp.image_ids"
    return r

getByFileName :: Connection -> Slug.Type -> Text -> IO (Maybe Image)
getByFileName conn slug fn = listToMaybe <$> query conn (selectBase <> " WHERE post_id = (SELECT id FROM posts WHERE slug = ?) AND (file_name = ? OR preview_file_name = ?)") (slug, fn, fn)

updateCaption :: Connection -> ImageId -> Text -> IO Image
updateCaption conn iid cap = do
    execute conn "UPDATE images SET caption = ? WHERE id = ?" (cap, iid)
    get' conn iid

getFiles :: Connection -> ImageId -> IO (Maybe (Text, Text))
getFiles conn iid = listToMaybe <$> query conn "SELECT file_name, preview_file_name FROM images WHERE id = ?" (Only iid)

delete :: Connection -> ImageId -> IO ()
delete conn iid = do
    mi <- get conn iid
    case mi of
        Nothing -> return ()
        Just i -> do
            let rc = imageRefCount i
            if rc > 1 then updateRC conn iid (rc - 1)
                      else doDelete
    where
        doDelete = do
            mfs <- getFiles conn iid
            case mfs of
                Nothing -> return ()
                Just (sfn, spn) -> do
                    removeFiles sfn spn
                    execute conn "DELETE FROM images WHERE id = ?" (Only iid)

updateRC :: Connection -> ImageId -> Int -> IO ()
updateRC conn iid rc = execute conn "UPDATE images SET ref_count = ? WHERE id = ?" (rc, iid)

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

upload :: Connection -> Config -> Slug.Type -> File BS.ByteString -> IO Image
upload conn conf slug (_, fi) = do
    mpi <- getPostInfo
    case mpi of
        Nothing -> error "Unknown post id"
        Just (pid, created) -> do
            let fh = contentHash fi
            moimg <- findByHash conn pid fh
            case moimg of
                Just img -> do
                    updateRC conn (imageId img) (succ . imageRefCount $ img)
                    return img
                Nothing -> doUpload fh pid created
    where
        getPostInfo = listToMaybe <$> query conn "SELECT id, created FROM posts WHERE slug = ?" (Only slug)
        fn = decodeUtf8 $ fileName fi
        pn = previewPrefix (images conf) <> fn
        width = CP.dynamicMap CP.imageWidth
        height = CP.dynamicMap CP.imageHeight
        dph = previewHeight $ images conf
        jq = jpegQuality $ images conf
        mime = decodeUtf8 $ fileContentType fi
        timeYear = toText . timeToYear
        cleanup sfn spn m = do
            removeFiles sfn spn
            error (unpack m)
        doUpload fh pid created = do
            let std = destDir (main conf) <> timeYear created
            let stp = std <> "/" <> Slug.unSlug slug
            checkCreateDir stp
            let sfn = stp <> "/" <> fn
            let spn = stp <> "/" <> pn
            (md, img) <- prepareImage sfn fi
            (pw, ph) <- createPreview jq dph spn md img
            fs <- getSize sfn
            ps <- getSize spn
            now <- getCurrentTime
            let uBase = urlBase (main conf) <> "/" <> timeYear created <> "/" <> Slug.unSlug slug
            let u = uBase <> "/" <> fn
            let pu = uBase <> "/" <> pn
            ri <- createImage conn (ImageInfo pid "" fn fs fh (width img) (height img) mime u pn ps pw ph pu now Nothing 1)
            case ri of
                Left e -> cleanup sfn spn e
                Right i -> return i

createImage :: Connection -> ImageInfo -> IO (Either Text Image)
createImage conn info = do
    miid <- fmap fromOnly . listToMaybe <$> query conn "INSERT INTO images (post_id, caption, file_name, file_size, file_hash, width, height, mime_type, url, preview_file_name, preview_file_size, preview_width, preview_height, preview_url, created, updated, ref_count) VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?) RETURNING id" info :: IO (Maybe ImageId)
    case miid of
        Nothing -> return $ Left "Cannot insert image into the DB"
        Just iid -> return $ Right (Image iid info)

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

findByHash :: Connection -> PostId -> Int -> IO (Maybe Image)
findByHash conn pid fh = listToMaybe <$> query conn (selectBase <> " WHERE post_id = ? AND file_hash = ?") (pid, fh)
