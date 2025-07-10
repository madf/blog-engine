module Madf.Blog.Config
    ( Config (..)
    , DBConfig (..)
    , ImagesConfig (..)
    , AdminConfig (..)
    , readFile
    , defaultConfig
    ) where

import Prelude hiding (readFile)
import Data.Text
import qualified Data.Text.IO as DTIO
import Data.Ini.Config
import Data.Password.Argon2
import Madf.Blog.Utils

data Config = Config
    { db     :: !DBConfig
    , images :: !ImagesConfig
    , admin  :: !AdminConfig
    } deriving (Show)

newtype DBConfig = DBConfig
    { path :: Text
    } deriving (Show)

data ImagesConfig = ImagesConfig
    { storageDir    :: !Text
    , previewHeight :: !Int
    , jpegQuality   :: !Int
    , previewPrefix :: !Text
    , urlBase       :: !Text
    } deriving (Show)

data AdminConfig = AdminConfig
    { login        :: !Text
    , passwordHash :: !(PasswordHash Argon2)
    } deriving (Show)

readFile :: Text -> IO (Either Text Config)
readFile file = do
    t <- DTIO.readFile $ unpack file
    return $ mapLeft pack (parseIniFile t parser)

defaultPasswordHash :: PasswordHash Argon2
defaultPasswordHash = PasswordHash "$argon2id$v=19$m=65536,t=2,p=1$OjYULa8hWb3ztYoAnzfWGA$ujHAUbMCCIYGqA1ytEq4gRgOFoAJ3dVU1lzOIL6f4y8"

defaultConfig :: Config
defaultConfig = Config
    (DBConfig "test.db")
    (ImagesConfig "data/images" 300 100 "preview-" "/data/images")
    (AdminConfig "admin" defaultPasswordHash)

parser :: IniParser Config
parser = do
    dc <- section "database" $ do
        p <- pack <$> fieldOf "path" string
        return $ DBConfig p
    ic <- section "images" $ do
        sd <- pack <$> fieldOf "storage_dir" string
        ph <- fieldOf "preview_height" number
        jq <- fieldOf "jpeg_quality" number
        pp <- pack <$> fieldOf "preview_prefix" string
        ub <- pack <$> fieldOf "url_base" string
        return $ ImagesConfig sd ph jq pp ub
    ac <- section "admin" $ do
        l <- pack <$> fieldOf "login" string
        ph <- PasswordHash . pack <$> fieldOf "password_hash" string
        return $ AdminConfig l ph
    return $ Config dc ic ac
