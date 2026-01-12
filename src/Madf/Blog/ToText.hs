module Madf.Blog.ToText
    ( ToText (..)
    ) where

import Data.Text
import Data.Time

class ToText a
    where
        toText :: a -> Text

instance ToText Int where
    toText = pack . show

instance ToText NominalDiffTime where
    toText = pack . show
