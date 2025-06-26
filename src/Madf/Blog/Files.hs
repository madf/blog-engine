module Madf.Blog.Files
    ( getSize
    , removeIfExists
    ) where

import Prelude hiding (catch)
import System.Directory
import Control.Exception
import System.IO
import System.IO.Error hiding (catch)
import Data.Text

getSize :: Text -> IO Int
getSize f = fromInteger <$> withFile (unpack f) ReadMode hFileSize

removeIfExists :: Text -> IO ()
removeIfExists f = removeFile (unpack f) `catch` handleExists
  where handleExists e
          | isDoesNotExistError e = return ()
          | otherwise = throwIO e
