module Madf.Blog.ToText
    ( ToText (..)
    ) where

import Data.Text

class ToText a
    where
        toText :: a -> Text
