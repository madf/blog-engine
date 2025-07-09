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
    , imageCaption
    , imageURL
    , imagePreviewWidth
    , imagePreviewHeight
    , imagePreviewURL
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
    } deriving (Show, Generic, FromRow, ToRow)

instance FromJSON ImageInfo
    where
        parseJSON = withObject "Madf.Blog.ImageInfo" $ \o -> ImageInfo
            <$> o .: "post_id"
            <*> o .: "caption"
            <*> o .: "file_name"
            <*> o .: "file_size"
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

imagePreviewFileName :: Image -> Text
imagePreviewFileName = imageInfoPreviewFileName . imageInfo

imagePreviewWidth :: Image -> Int
imagePreviewWidth = imageInfoPreviewWidth . imageInfo

imagePreviewHeight :: Image -> Int
imagePreviewHeight = imageInfoPreviewHeight . imageInfo

imagePreviewURL :: Image -> Text
imagePreviewURL = imageInfoPreviewURL . imageInfo

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

data UFInfo = UFInfo !Text !Int64 !Int !Int !Text !Text !Int64 !Int !Int !UTCTime !ImageId deriving (Show, Generic, ToRow)

get :: Connection -> ImageId -> IO (Maybe Image)
get conn iid = listToMaybe <$> query conn "SELECT id, post_id, caption, file_name, file_size, file_hash, width, height, mime_type, preview_file_name, preview_file_size, preview_width, preview_height, created, updated FROM images WHERE id = ?" (Only iid)

get' :: Connection -> ImageId -> IO Image
get' conn iid = do
    mr <- get conn iid
    case mr of
        Just img -> return img
        Nothing -> error "Unknown image id"

updateFile :: Connection -> Config -> ImageId -> File BS.ByteString -> IO Image
updateFile conn conf iid (_, fi) = do
    mimg <- get conn iid
    case mimg of
        Nothing -> error "Unknown image id"
        Just oimg -> do
            let pid = imagePostId oimg
            (md, img) <- prepareImage (sfn pid) fi
            fh <- fileHash (sfn pid)
            if fh == imageFileHash oimg then return oimg
                                        else doUpdate md img oimg fh
    where
        fn = decodeUtf8 $ fileName fi
        pn = previewPrefix (images conf) <> fn
        std = storageDir $ images conf
        stp pid = std <> toText pid
        sfn pid = stp pid <> "/" <> fn
        spn pid = stp pid <> "/" <> pn
        width = CP.dynamicMap CP.imageWidth
        height = CP.dynamicMap CP.imageHeight
        dph = previewHeight $ images conf
        jq = jpegQuality $ images conf
        mime = decodeUtf8 $ fileContentType fi
        doUpdate md img oimg fh = do
            removeFiles (imageFileName oimg) (imagePreviewFileName oimg)
            let pid = imagePostId oimg
            (pw, ph) <- createPreview jq dph (spn pid) md img
            fs <- getSize (sfn pid)
            ps <- getSize (spn pid)
            now <- getCurrentTime
            execute conn "UPDATE images SET file_name = ?, file_size = ?, file_hash = ?, width = ?, height = ?, mime_type = ?, preview_file_name = ?, preview_file_size = ?, preview_width = ?, preview_height = ?, updated = ? WHERE id = ?" (UFInfo fn fs fh (width img) (height img) mime pn ps pw ph now iid)
            get' conn iid

updateCaption :: Connection -> ImageId -> Text -> IO Image
updateCaption conn iid cap = do
    execute conn "UPDATE images SET caption = ? WHERE id = ?" (cap, iid)
    get' conn iid

getFiles :: Connection -> ImageId -> IO (Maybe (Text, Text))
getFiles conn iid = listToMaybe <$> query conn "SELECT file_name, preview_file_name FROM images WHERE id = ?" (Only iid)

delete :: Connection -> ImageId -> IO ()
delete conn iid = do
    mfs <- getFiles conn iid
    case mfs of
        Nothing -> return ()
        Just (sfn, spn) -> do
            removeFiles sfn spn
            execute conn "DELETE FROM images WHERE id = ?" (Only iid)

listByPost :: Connection -> PostId -> IO [Image]
listByPost conn pid = query conn "SELECT id, post_id caption, file_name, file_size, file_hash, width, height, mime_type, preview_file_name, preview_file_size, preview_width, preview_height, created, updated FROM images WHERE post_id = ?" (Only pid)

deleteByPost :: Connection -> PostId -> IO ()
deleteByPost conn pid = do
    fs <- query conn "SELECT file_name, preview_file_name FROM images WHERE post_id = ?" (Only pid)
    mapM_ (uncurry removeFiles) fs
    execute conn "DELETE FROM images WHERE post_id = ?" (Only pid)

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
    fh <- fileHash sfn
    moimg <- findByHash pid fh
    case moimg of
        Just img -> return img
        Nothing -> doUpload md img fh
    where
        checkPId = Prelude.any fromOnly <$> query conn "SELECT EXISTS (SELECT 1 FROM posts WHERE id = ?)" (Only pid)
        fn = decodeUtf8 $ fileName fi
        pn = previewPrefix (images conf) <> fn
        std = storageDir $ images conf
        stp = std <> "/" <> toText pid
        sfn = stp <> "/" <> fn
        spn = stp <> "/" <> pn
        uBase = urlBase (images conf) <> "/" <> toText pid
        u = uBase <> "/" <> fn
        pu = uBase <> "/" <> pn
        width = CP.dynamicMap CP.imageWidth
        height = CP.dynamicMap CP.imageHeight
        dph = previewHeight $ images conf
        jq = jpegQuality $ images conf
        mime = decodeUtf8 $ fileContentType fi
        cleanup m = do
            removeFiles sfn spn
            error (unpack m)
        doUpload = do
            (pw, ph) <- createPreview jq dph spn md img
            fs <- getSize sfn
            ps <- getSize spn
            now <- getCurrentTime
            ri <- createImage conn (ImageInfo pid "" fn fs (width img) (height img) mime u pn ps pw ph pu now Nothing)
            case ri of
                Left e -> cleanup e
                Right i -> return i

createImage :: Connection -> ImageInfo -> IO (Either Text Image)
createImage conn info = do
    miid <- fmap fromOnly . listToMaybe <$> query conn "INSERT INTO images (post_id, caption, file_name, file_size, file_hash, width, height, mime_type, url, preview_file_name, preview_file_size, preview_width, preview_height, preview_url, created, updated) VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?) RETURNING id" info :: IO (Maybe ImageId)
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
