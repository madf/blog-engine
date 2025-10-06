module Madf.Blog.Error
    ( BlogError (..)
    , throwBlogError
    ) where

import Data.Text
import Control.Exception

data BlogError
    = DatabaseError Text
    | PostNotFound Text
    | ImageNotFound Text
    | ImageError Text
    | SchemaError Text
    | ConfigError Text
    deriving (Show)

instance Exception BlogError

throwBlogError :: BlogError -> IO a
throwBlogError = throwIO
