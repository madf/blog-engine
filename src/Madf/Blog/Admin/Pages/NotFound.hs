module Madf.Blog.Admin.Pages.NotFound
    ( notFound
    ) where

import Data.Text
import Lucid

notFound :: Text -> Html ()
notFound m = do
    h1_ "Not found"
    p_ $ toHtml m
