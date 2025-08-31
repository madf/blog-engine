{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Madf.Blog.Time
    ( Year (..)
    , unYear
    , timeToText
    , timeToYear
    , currentYear
    , splitDate
    , yearStart
    ) where

import Data.Time.Clock
import Data.Time.Format
import Data.Time.Calendar.OrdinalDate (toOrdinalDate, fromOrdinalDate)
import Data.Text
import Database.SQLite.Simple.FromField
import Database.SQLite.Simple.ToField
import Data.Aeson
import Web.Scotty
import Lucid
import Madf.Blog.ToText

newtype Year = Year Int deriving (Show, Eq, Enum, FromField, ToField, FromJSON, ToJSON, Parsable)

instance ToHtml Year
    where
        toHtml = toHtml . toText
        toHtmlRaw = toHtmlRaw . toText

instance ToText Year
    where
        toText = pack . show . unYear

unYear :: Year -> Int
unYear (Year v) = v

timeToText :: UTCTime -> Text
timeToText = ft "%F %T"

timeToYear :: UTCTime -> Year
timeToYear = Year . fromIntegral . fst . toOrdinalDate . utctDay

currentYear :: IO Year
currentYear = timeToYear <$> getCurrentTime

splitDate :: UTCTime -> (Text, Text)
splitDate t = (ft "%b" t, ft "%d" t)

yearStart :: Year -> UTCTime
yearStart (Year v) = UTCTime (fromOrdinalDate (fromIntegral v) 0) 0

ft :: String -> UTCTime -> Text
ft f t = pack (formatTime defaultTimeLocale f t)
