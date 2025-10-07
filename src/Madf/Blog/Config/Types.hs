module Madf.Blog.Config.Types
    ( Config (..)
    , MainConfig (..)
    , DBConfig (..)
    , ImagesConfig (..)
    , AdminConfig (..)
    , LoggingConfig (..)
    , LogDestination (..)
    , ServerConfig (..)
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
    , server  :: !ServerConfig
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

newtype LoggingConfig = LoggingConfig
    { destination :: LogDestination
    } deriving (Show)

data ServerConfig = ServerConfig
    { port  :: !Int
    , host  :: !Text
    , debug :: !Bool
    } deriving (Show)
