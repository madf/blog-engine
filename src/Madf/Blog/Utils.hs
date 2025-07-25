{-# LANGUAGE FlexibleContexts #-}

module Madf.Blog.Utils
    ( timeToText
    , splitDate
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
currentYear = ft "%Y" <$> getCurrentTime

timeToText :: UTCTime -> Text
timeToText = ft "%F %T"

splitDate :: UTCTime -> (Text, Text)
splitDate t = (ft "%b" t, ft "%d" t)

ft :: String -> UTCTime -> Text
ft f t = pack (formatTime defaultTimeLocale f t)

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
