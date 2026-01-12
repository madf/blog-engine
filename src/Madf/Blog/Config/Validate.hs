module Madf.Blog.Config.Validate
    ( ValidationError (..)
    , validate
    ) where

import Data.Text (Text)
import Data.Text qualified as T
import System.Directory
import System.FilePath
import Madf.Blog.Config.Types
import Madf.Blog.ToText
import qualified Madf.Blog.JWT as JWT
import qualified Madf.Blog.Job as Job

-- Validation errors
data ValidationError
    = DirectoryNotFound Text
    | DirectoryNotWritable Text
    | ParentDirectoryNotFound Text
    | InvalidValue Text Text  -- field name, error message
    deriving (Show, Eq)

-- Validation function
validate :: Config -> IO [ValidationError]
validate conf = do
    dirErrors <- validateDirectories conf
    let valueErrors = validateValues conf
    return $ dirErrors ++ valueErrors

validateDirectories :: Config -> IO [ValidationError]
validateDirectories conf = do
    destDirErrs <- checkDirectory "main.dest_dir" (destDir $ main conf)
    dbDirErrs <- checkParentDirectory "database.path" (path $ db conf)
    jwtPathErrs <- checkJWTKeyPath "jwt.path" (T.pack . JWT.path $ jwt conf)
    logErrs <- validateLogDestination "logging.destination" (destination $ logging conf)
    return $ destDirErrs ++ dbDirErrs ++ jwtPathErrs ++ logErrs

checkDirectory :: Text -> Text -> IO [ValidationError]
checkDirectory field dir = do
    exists <- doesDirectoryExist (T.unpack dir)
    if exists
        then do
            perms <- getPermissions (T.unpack dir)
            if writable perms
                then return []
                else return [DirectoryNotWritable $ field <> ": " <> dir]
        else return [DirectoryNotFound $ field <> ": " <> dir]

checkParentDirectory :: Text -> Text -> IO [ValidationError]
checkParentDirectory field filePath = do
    let parentDir = takeDirectory (T.unpack filePath)
    exists <- doesDirectoryExist parentDir
    if exists
        then do
            perms <- getPermissions parentDir
            if writable perms
                then return []
                else return [DirectoryNotWritable $ field <> " parent: " <> T.pack parentDir]
        else return [ParentDirectoryNotFound $ field <> " parent: " <> T.pack parentDir]

checkJWTKeyPath :: Text -> Text -> IO [ValidationError]
checkJWTKeyPath = checkParentDirectory

validateLogDestination :: Text -> LogDestination -> IO [ValidationError]
validateLogDestination field dest = case dest of
    Stdout -> return []
    Syslog -> return []
    File fp -> checkParentDirectory field (T.pack fp)

validateValues :: Config -> [ValidationError]
validateValues conf = concat
    [ validatePositive "main.num_posts" (numPosts $ main conf)
    , validateJpegQuality "images.jpeg_quality" (jpegQuality $ images conf)
    , validateNonEmpty "images.preview_prefix" (previewPrefix $ images conf)
    , validatePositive "images.preview_height" (previewHeight $ images conf)
    , validateNonEmpty "admin.login" (login $ admin conf)
    , validateNonEmpty "jwt.key_issuer" (JWT.keyIssuer $ jwt conf)
    , validatePort "server.port" (port $ server conf)
    , validateNonEmpty "server.host" (host $ server conf)
    , validatePositive "job.cleanup_interval" (Job.cleanupInterval $ job conf)
    , validatePositive "job.max_ttl" (Job.maxTTL $ job conf)
    , validatePositive "job.max_concurrency" (Job.maxConcurrency $ job conf)
    ]

validatePositive :: (Num a, Ord a, ToText a) => Text -> a -> [ValidationError]
validatePositive field n
    | n > 0     = []
    | otherwise = [InvalidValue field $ "Value must be positive, got: " <> toText n]

validateJpegQuality :: Text -> Int -> [ValidationError]
validateJpegQuality field n
    | n >= 1 && n <= 100 = []
    | otherwise          = [InvalidValue field $ "JPEG quality must be between 1 and 100, got: " <> toText n]

validateNonEmpty :: Text -> Text -> [ValidationError]
validateNonEmpty field t
    | T.null t  = [InvalidValue field "Value cannot be empty"]
    | otherwise = []

validatePort :: Text -> Int -> [ValidationError]
validatePort field p
    | p >= 1 && p <= 65535 = []
    | otherwise            = [InvalidValue field $ "Port must be between 1 and 65535, got: " <> toText p]
