module Madf.Blog.Config.Types
    ( Config (..)
    , MainConfig (..)
    , DBConfig (..)
    , ImagesConfig (..)
    , AdminConfig (..)
    , LoggingConfig (..)
    , LogDestination (..)
    ) where

import Data.Text
import Data.Password.Argon2
import qualified Madf.Blog.JWT as JWT

data Config = Config
    { main    :: !MainConfig
    , db      :: !DBConfig
    , images  :: !ImagesConfig
    , admin   :: !AdminConfig
    , jwt     :: !JWT.Config
    , logging :: !LoggingConfig
    } deriving (Show)

data LogDestination = Stdout | File FilePath | Syslog
    deriving (Show, Eq)

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

data LoggingConfig = LoggingConfig
    { debug       :: !Bool
    , destination :: !LogDestination
    } deriving (Show)
