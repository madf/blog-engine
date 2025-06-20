module Madf.Blog.Utils
    ( lucid
    , timeToText
    ) where

import Data.Text
import Data.Time.Clock
import Data.Time.Format
import Web.Scotty
import Lucid

lucid :: Html a -> ActionM ()
lucid h = do
    setHeader "Content-Type" "text/html"
    raw (renderBS h)

timeToText :: UTCTime -> Text
timeToText = pack . formatTime defaultTimeLocale "%F %T"
