{-# LANGUAGE FlexibleContexts #-}

module Madf.Blog.Utils
    ( mapLeft
    , contentHash
    , fileHash
    ) where

import Data.Text
import Data.ByteString qualified as BS
import Data.Digest.XXHash.FFI
import Data.Hashable
import Network.Wai.Parse (FileInfo (..))

mapLeft :: (a -> b) -> Either a c -> Either b c
mapLeft f = \case
    Left v -> Left (f v)
    Right v -> Right v

contentHash :: Hashable (XXH3 a) => FileInfo a -> Int
contentHash = hash . XXH3 . fileContent

fileHash :: Text -> IO Int
fileHash f = do
    c <- BS.readFile $ unpack f
    return $ hash (XXH3 c)
