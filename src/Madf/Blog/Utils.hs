{-# LANGUAGE FlexibleContexts #-}

module Madf.Blog.Utils
    ( timeToText
    , currentYear
    , mapLeft
    , contentHash
    , fileHash
    ) where

import Data.Text
import qualified Data.ByteString as BS
import Data.Time.Clock
import Data.Time.Format
import Data.Digest.XXHash.FFI
import Data.Hashable
import Network.Wai.Parse (FileInfo (..))

currentYear :: IO Text
currentYear = pack . formatTime defaultTimeLocale "%Y" <$> getCurrentTime

timeToText :: UTCTime -> Text
timeToText = pack . formatTime defaultTimeLocale "%F %T"

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
