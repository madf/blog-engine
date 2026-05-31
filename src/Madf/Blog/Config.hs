module Madf.Blog.Config
    ( module Madf.Blog.Config.Types
    , readFile
    , defaultConfig
    , module Madf.Blog.Config.Validate
    ) where

import Prelude hiding (readFile)
import Data.Text
import Data.Text qualified as T
import qualified Data.Text.IO as DTIO
import Data.Ini.Config
import Data.Maybe (fromMaybe)
import Data.Password.Argon2
import qualified Madf.Blog.JWT as JWT
import qualified Madf.Blog.Job as Job
import Madf.Blog.Image.Scale qualified as Scale
import Madf.Blog.Config.Types
import Madf.Blog.Config.Validate

readFile :: Text -> IO (Either Text Config)
readFile file = do
    t <- DTIO.readFile $ unpack file
    case parseIniFile t parser of
        Left e -> return $ Left (pack e)
        Right c -> do
            validationErrors <- validate c
            case validationErrors of
                [] -> return $ Right c
                errs -> return $ Left $ "Configuration validation failed:\n" <> formatValidationErrors errs

formatValidationErrors :: [ValidationError] -> Text
formatValidationErrors errs = T.intercalate "\n" (Prelude.map formatError errs)
  where
    formatError (DirectoryNotFound dir) = "  - Directory not found: " <> dir
    formatError (DirectoryNotWritable dir) = "  - Directory not writable: " <> dir
    formatError (ParentDirectoryNotFound dir) = "  - Parent directory not found: " <> dir
    formatError (InvalidValue fieldName msg) = "  - " <> fieldName <> ": " <> msg

defaultPasswordHash :: PasswordHash Argon2
defaultPasswordHash = PasswordHash "$argon2id$v=19$m=65536,t=2,p=1$OjYULa8hWb3ztYoAnzfWGA$ujHAUbMCCIYGqA1ytEq4gRgOFoAJ3dVU1lzOIL6f4y8"

defaultConfig :: Config
defaultConfig = Config
    (MainConfig "/var/www/mbe.site" 10)
    (DBConfig "/var/lib/mbe/storage.db")
    (ImagesConfig 300 100 "preview-" Scale.Direct)
    (AdminConfig "admin" defaultPasswordHash)
    JWT.defaultConfig
    (LoggingConfig Stdout)
    (ServerConfig 3000 "127.0.0.1" False)
    Job.defaultConfig

parser :: IniParser Config
parser = do
    mc <- section "main" $ do
        dd <- pack <$> fieldOf "dest_dir" string
        np <- fieldOf "num_posts" number
        return $ MainConfig dd np
    dc <- section "database" $ do
        p <- pack <$> fieldOf "path" string
        return $ DBConfig p
    ic <- section "images" $ do
        ph <- fieldOf "preview_height" number
        jq <- fieldOf "jpeg_quality" number
        pp <- pack <$> fieldOf "preview_prefix" string
        sm <- fromMaybe Scale.Direct <$> fieldMbOf "scale_method" scaleMethodParser
        return $ ImagesConfig ph jq pp sm
    ac <- section "admin" $ do
        l <- pack <$> fieldOf "login" string
        ph <- PasswordHash . pack <$> fieldOf "password_hash" string
        return $ AdminConfig l ph
    jc <- section "jwt" JWT.configParser
    lc <- section "logging" $ do
        dest <- fieldOf "destination" logDestination
        return $ LoggingConfig dest
    sc <- section "server" $ do
        p <- fieldOf "port" number
        h <- pack <$> fieldOf "host" string
        d <- fieldOf "debug" flag
        return $ ServerConfig p h d
    jobc <- section "job" $ do
        ci <- fieldOf "cleanup_interval" number
        mttl <- fromInteger <$> fieldOf "max_ttl" number
        mccy <- fieldOf "max_concurrency" number
        return $ Job.Config ci mttl mccy
    return $ Config mc dc ic ac jc lc sc jobc

logDestination :: Text -> Either String LogDestination
logDestination = \case
    "stdout" -> Right Stdout
    "syslog" -> Right Syslog
    v -> if "file:" `T.isPrefixOf` v
         then Right . File . unpack $ T.drop 5 v
         else Left . unpack $ "Unknown log destination: '" <> v <> "'. Use 'stdout', 'syslog', or 'file:<path>'."

scaleMethodParser :: Text -> Either String Scale.Method
scaleMethodParser = \case
    "direct"   -> Right Scale.Direct
    "filtered" -> Right Scale.Filtered
    v -> Left . unpack $ "Unknown scale method: '" <> v <> "'. Use 'direct' or 'filtered'."
