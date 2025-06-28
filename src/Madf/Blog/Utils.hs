module Madf.Blog.Utils
    ( timeToText
    , mapLeft
    ) where

import Data.Text
import Data.Time.Clock
import Data.Time.Format

timeToText :: UTCTime -> Text
timeToText = pack . formatTime defaultTimeLocale "%F %T"

mapLeft :: (a -> b) -> Either a c -> Either b c
mapLeft f = \case
    Left v -> Left (f v)
    Right v -> Right v
