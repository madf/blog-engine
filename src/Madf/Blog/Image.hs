{-# LANGUAGE DeriveGeneric  #-}
{-# LANGUAGE DeriveAnyClass #-}

module Madf.Blog.Image
    ( Image (..)
    , list
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
    , regenPreview
    ) where

import GHC.Generics
import Data.Text
import Data.Text.Encoding
import Data.Time (getCurrentTime, UTCTime)
import Data.ByteString.Lazy qualified as BS
import Data.Int
import Data.Maybe
import Data.Aeson
import Database.SQLite.Simple
import Web.Scotty (File)
import Network.Wai.Parse (FileInfo (..))
import Codec.Picture qualified as CP
import Codec.Picture.Metadata qualified as CP
import Graphics.HsExif qualified as EXIF
import Madf.Blog.Image.Scale qualified as Scale
import Madf.Blog.Image.Orientation qualified as O
import Madf.Blog.Ids
import Madf.Blog.Config
import Madf.Blog.Files
import Madf.Blog.Time
import Madf.Blog.Utils
import Madf.Blog.Slug qualified as Slug
import Madf.Blog.ToText
import Madf.Blog.Error

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

list :: Connection -> IO [Image]
list conn = query_ conn selectBase

get :: Connection -> ImageId -> IO (Maybe Image)
get conn iid = listToMaybe <$> query conn (selectBase <> " WHERE id = ?") (Only iid)

get' :: Connection -> ImageId -> IO Image
get' conn iid = do
    mr <- get conn iid
    case mr of
        Just img -> return img
        Nothing -> throwBlogError (ImageNotFound $ "Unknown image id: " <> toText iid)

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
                    removeIfExists sfn
                    removeIfExists spn
                    execute conn "DELETE FROM images WHERE id = ?" (Only iid)

updateRC :: Connection -> ImageId -> Int -> IO ()
updateRC conn iid rc = execute conn "UPDATE images SET ref_count = ? WHERE id = ?" (rc, iid)

data PostInfo = PostInfo
    { piId      :: !PostId
    , piSlug    :: !Slug.Type
    , piYear    :: !Year
    }

getPostInfo :: Connection -> Slug.Type -> IO (Maybe PostInfo)
getPostInfo conn slug = do
    minf <- listToMaybe <$> query conn "SELECT id, created FROM posts WHERE slug = ?" (Only slug)
    case minf of
        Nothing -> return Nothing
        Just (i, c) -> return $ Just (PostInfo i slug (timeToYear c))

getPostInfoById :: Connection -> PostId -> IO (Maybe PostInfo)
getPostInfoById conn i = do
    minf <- listToMaybe <$> query conn "SELECT slug, created FROM posts WHERE id = ?" (Only i)
    case minf of
        Nothing -> return Nothing
        Just (s, c) -> return $ Just (PostInfo i s (timeToYear c))

upload :: Connection -> Config -> Slug.Type -> File BS.ByteString -> IO Image
upload conn conf slug (_, fi) = do
    mpi <- getPostInfo conn slug
    case mpi of
        Nothing -> throwBlogError (PostNotFound $ "Unknown post slug: " <> toText slug)
        Just pinf -> do
            let fh = contentHash fi
            moimg <- findByHash conn (piId pinf) fh
            case moimg of
                Just img -> do
                    updateRC conn (imageId img) (succ . imageRefCount $ img)
                    return img
                Nothing -> doUpload conn conf fh pinf fi

doUpload :: Connection -> Config -> Int -> PostInfo -> FileInfo BS.ByteString -> IO Image
doUpload conn conf fh pinf fi = do
    imageData <- decodeImage (fileContent fi) (decodeUtf8 $ fileName fi)
    let mime = decodeUtf8 $ fileContentType fi
    let previewImage = createPreview conf imageData orientation
    checkCreateDir postDir
    imageFileInfo <- saveOriginal postDir fi
    previewImageFileInfo <- saveImage conf postDir previewImage
    res <- putInDatabase conn pinf fh mime imageData previewImage (fileSize imageFileInfo) (fileSize previewImageFileInfo)
    case res of
        Left e -> do
            removeIfExists (filePath imageFileInfo)
            removeIfExists (filePath previewImageFileInfo)
            throwBlogError (DatabaseError e)
        Right i -> return i
    where
        yearDir = destDir (main conf) <> "/" <> toText (piYear pinf)
        postDir = yearDir <> "/" <> toText (piSlug pinf)
        eexif = EXIF.parseExif $ fileContent fi
        orientation = either (const Nothing) EXIF.getOrientation eexif

data ImageData = ImageData
    { idImage    :: !CP.DynamicImage
    , idFormat   :: !CP.SourceFormat
    , idWidth    :: !Int
    , idHeight   :: !Int
    , idFileName :: !Text
    }

decodeImage :: BS.ByteString -> Text -> IO ImageData
decodeImage bytes fn = case CP.decodeImageWithMetadata (BS.toStrict bytes) of
    Left e -> throwBlogError (ImageError $ pack e)
    Right (img, md) -> case CP.lookup CP.Format md of
        Nothing -> throwBlogError $ ImageError "Unknown image format"
        Just f -> do
            let w = CP.dynamicMap CP.imageWidth img
            let h = CP.dynamicMap CP.imageHeight img
            return $ ImageData img f w h fn

willTranspose :: Maybe EXIF.ImageOrientation -> Bool
willTranspose = \case
    Just (EXIF.MirrorRotation EXIF.MinusNinety) -> True
    Just (EXIF.Rotation EXIF.MinusNinety)       -> True
    Just (EXIF.MirrorRotation EXIF.Ninety)      -> True
    Just (EXIF.Rotation EXIF.Ninety)            -> True
    _                                           -> False

createPreview :: Config -> ImageData -> Maybe EXIF.ImageOrientation -> ImageData
createPreview conf idata orientation = ImageData pimg (idFormat idata) pw ph fn
    where
        iconf = images conf
        dh = previewHeight iconf
        fn = previewPrefix iconf <> idFileName idata
        -- Swap target dimensions if image will be transposed
        (tw, th) = if willTranspose orientation
                   then (dh, h * dh `div` w)  -- Use h/w aspect ratio for transposed images
                   else (w * dh `div` h, dh)
        pimg = maybeRotate $ Scale.scale (scaleMethod iconf) tw th img
        maybeRotate = maybe id O.normalize orientation
        img = idImage idata
        w = idWidth idata
        h = idHeight idata
        pw = CP.dynamicMap CP.imageWidth pimg
        ph = CP.dynamicMap CP.imageHeight pimg

regenPreview :: Connection -> Config -> Image -> IO ()
regenPreview conn conf img = do
    let pid = imagePostId img
        fn = imageFileName img
        pfn = imagePreviewFileName img
    mpinf <- getPostInfoById conn pid
    case mpinf of
        Nothing -> throwBlogError (PostNotFound $ "Post not found for image " <> toText (imageId img) <> ", '" <> fn <> "', with post id " <> toText pid)
        Just (PostInfo _ s y) -> do
            let postDir = destDir (main conf) <> "/" <> toText y <> "/" <> toText s
                fp = postDir <> "/" <> fn
                pfp = postDir <> "/" <> pfn
            bytes <- BS.readFile (unpack fp)
            imageData <- decodeImage bytes fn
            let eexif = EXIF.parseExif bytes
            let orientation = either (const Nothing) EXIF.getOrientation eexif
            let previewImage = createPreview conf imageData orientation
            removeIfExists pfp
            previewImageFileInfo <- saveImage conf postDir previewImage
            now <- getCurrentTime
            execute conn
                "UPDATE images SET preview_file_name = ?, preview_file_size = ?, preview_width = ?, preview_height = ?, updated = ? WHERE id = ?"
                (idFileName previewImage, fileSize previewImageFileInfo, idWidth previewImage, idHeight previewImage, now, imageId img)

data ImageFileInfo = ImageFileInfo
    { filePath :: !Text
    , fileSize :: !Int64
    }

saveOriginal :: Text -> FileInfo BS.ByteString -> IO ImageFileInfo
saveOriginal dir fi = do
    let fp = dir <> "/" <> decodeUtf8 (fileName fi)
    BS.writeFile (unpack fp) (fileContent fi)
    s <- getSize fp
    return $ ImageFileInfo fp s

saveImage :: Config -> Text -> ImageData -> IO ImageFileInfo
saveImage conf dir idata = do
    case idFormat idata of
        CP.SourceJpeg -> CP.saveJpgImage jq (unpack fp) img
        CP.SourcePng  -> CP.savePngImage (unpack fp) img
        v             -> throwBlogError $ ImageError ("Unsupported image format: " <> pack (show v))
    s <- getSize fp
    return $ ImageFileInfo fp s
    where
        jq = jpegQuality $ images conf
        fp = dir <> "/" <> idFileName idata
        img = idImage idata

putInDatabase :: Connection -> PostInfo -> Int -> Text -> ImageData -> ImageData -> Int64 -> Int64 -> IO (Either Text Image)
putInDatabase conn pinf fh mime imgData pimgData is pis = do
    now <- getCurrentTime
    let info = getInfo now
    miid <- fmap fromOnly . listToMaybe <$> query conn "INSERT INTO images (post_id, caption, file_name, file_size, file_hash, width, height, mime_type, url, preview_file_name, preview_file_size, preview_width, preview_height, preview_url, created, updated, ref_count) VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?) RETURNING id" info :: IO (Maybe ImageId)
    case miid of
        Nothing -> return $ Left "Cannot insert image into the DB"
        Just iid -> return $ Right (Image iid info)
    where
        uBase = "/blog/" <> toText (piYear pinf) <> "/" <> toText (piSlug pinf)
        url = uBase <> "/" <> idFileName imgData
        purl = uBase <> "/" <> idFileName pimgData
        getInfo now = ImageInfo (piId pinf) ""
                                (idFileName imgData) is fh (idWidth imgData) (idHeight imgData) mime url
                                (idFileName pimgData) pis (idWidth pimgData) (idHeight pimgData) purl
                                now Nothing 1

findByHash :: Connection -> PostId -> Int -> IO (Maybe Image)
findByHash conn pid fh = listToMaybe <$> query conn (selectBase <> " WHERE post_id = ? AND file_hash = ?") (pid, fh)
