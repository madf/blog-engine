module Madf.Blog.Utils
    ( timeToText
    , mapLeft
    , fileHash
    ) where

import Data.Text
import qualified Data.ByteString as BS
import Data.Time.Clock
import Data.Time.Format
import Data.Digest.XXHash.FFI
import Data.Hashable

timeToText :: UTCTime -> Text
timeToText = pack . formatTime defaultTimeLocale "%F %T"

mapLeft :: (a -> b) -> Either a c -> Either b c
mapLeft f = \case
    Left v -> Left (f v)
    Right v -> Right v

fileHash :: Text -> IO Int
fileHash f = do
    c <- BS.readFile $ unpack f
    return $ hash (XXH3 c)
