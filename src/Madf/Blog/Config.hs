module Madf.Blog.Config
    ( Config (..)
    , DBConfig (..)
    , ImagesConfig (..)
    , AdminConfig (..)
    , readFile
    ) where

import Prelude hiding (readFile)
import Data.Text
import qualified Data.Text.IO as DTIO
import Data.Ini.Config
import Madf.Blog.Utils

data Config = Config
    { db     :: !DBConfig
    , images :: !ImagesConfig
    , admin  :: !AdminConfig
    } deriving (Show)

data DBConfig = DBConfig
    { path :: !Text
    } deriving (Show)

data ImagesConfig = ImagesConfig
    { storageDir    :: !Text
    , previewHeight :: !Int
    , jpegQuality   :: !Int
    } deriving (Show)

data AdminConfig = AdminConfig
    { login        :: !Text
    , passwordHash :: !Text
    } deriving (Show)

readFile :: Text -> IO (Either Text Config)
readFile file = do
    t <- DTIO.readFile $ unpack file
    return $ mapLeft pack (parseIniFile t parser)

parser :: IniParser Config
parser = do
    dc <- section "database" $ do
        p <- pack <$> fieldOf "path" string
        return $ DBConfig p
    ic <- section "images" $ do
        sd <- pack <$> fieldOf "storage_dir" string
        ph <- fieldOf "preview_height" number
        jq <- fieldOf "jpeg_quality" number
        return $ ImagesConfig sd ph jq
    ac <- section "admin" $ do
        l <- pack <$> fieldOf "login" string
        ph <- pack <$> fieldOf "password_hash" string
        return $ AdminConfig l ph
    return $ Config dc ic ac
