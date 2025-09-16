module Madf.Blog.Config
    ( Config (..)
    , MainConfig (..)
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
import qualified Madf.Blog.JWT as JWT
import Madf.Blog.Utils

data Config = Config
    { main   :: !MainConfig
    , db     :: !DBConfig
    , images :: !ImagesConfig
    , admin  :: !AdminConfig
    , jwt    :: !JWT.Config
    } deriving (Show)

data MainConfig = MainConfig
    { destDir  :: !Text
    , numPosts :: !Int
    } deriving (Show)

newtype DBConfig = DBConfig
    { path :: Text
    } deriving (Show)

data ImagesConfig = ImagesConfig
    { previewHeight :: !Int
    , jpegQuality   :: !Int
    , previewPrefix :: !Text
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
    (MainConfig "/var/www/mbe.site" 10)
    (DBConfig "/var/lib/mbe/storage.db")
    (ImagesConfig 300 100 "preview-")
    (AdminConfig "admin" defaultPasswordHash)
    JWT.defaultConfig

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
    return $ Config mc dc ic ac jc
