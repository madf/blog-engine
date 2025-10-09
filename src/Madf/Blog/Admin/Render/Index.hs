module Madf.Blog.Admin.Render.Index
    ( index
    , yearIndex
    ) where

import Lucid
import Madf.Blog.Post.View qualified as Post
import Madf.Blog.Time
import Madf.Blog.ToText
import Madf.Blog.Admin.Render.Excerpt as Excerpt

index :: Year -> [Post.Post] -> Html ()
index cy posts = do
    mapM_ Excerpt.render posts
    with div_ [class_ "year-link"] $ do
        p_ $ do
            "More posts in "
            with a_ [href_ ("/blog/" <> toText cy)] (toHtml cy)

yearIndex :: [Post.Post] -> Html ()
yearIndex = mapM_ Excerpt.render
