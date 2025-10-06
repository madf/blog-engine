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
import Data.Password.Argon2
import qualified Madf.Blog.JWT as JWT
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
    (ImagesConfig 300 100 "preview-")
    (AdminConfig "admin" defaultPasswordHash)
    JWT.defaultConfig
    (LoggingConfig Info)

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
        return $ ImagesConfig ph jq pp
    ac <- section "admin" $ do
        l <- pack <$> fieldOf "login" string
        ph <- PasswordHash . pack <$> fieldOf "password_hash" string
        return $ AdminConfig l ph
    jc <- section "jwt" JWT.configParser
    lc <- section "logging" $ do
        lvl <- fieldOf "level" logLevel
        return $ LoggingConfig lvl
    return $ Config mc dc ic ac jc lc

logLevel :: Text -> Either String LogLevel
logLevel = \case
    "debug" -> Right Debug
    "info" -> Right Info
    "warning" -> Right Warning
    "error" -> Right Error
    v -> Left . unpack $ "Unknown log level: '" <> v <> "'."
