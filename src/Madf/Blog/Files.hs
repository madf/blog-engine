module Madf.Blog.Files
    ( getSize
    , removeIfExists
    , checkCreateDir
    ) where

import System.Directory
import Control.Exception
import System.IO
import System.IO.Error
import Data.Text
import Data.Int

getSize :: Text -> IO Int64
getSize f = fromInteger <$> withFile (unpack f) ReadMode hFileSize

removeIfExists :: Text -> IO ()
removeIfExists f = removeFile (unpack f) `catch` handleExists
  where handleExists e
          | isDoesNotExistError e = return ()
          | otherwise = throwIO e

checkCreateDir :: Text -> IO ()
checkCreateDir p = createDirectoryIfMissing True (unpack p)
