module Madf.Blog.Config.Types
    ( Config (..)
    , MainConfig (..)
    , DBConfig (..)
    , ImagesConfig (..)
    , AdminConfig (..)
    ) where

import Data.Text
import Data.Password.Argon2
import qualified Madf.Blog.JWT as JWT

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
