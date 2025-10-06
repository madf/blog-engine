module Madf.Blog.Admin.Pages.Contents
    ( draw
    ) where

import Lucid
import Madf.Blog.Contents
import Madf.Blog.Post.View qualified as Post
import Madf.Blog.Time
import Madf.Blog.ToText

draw :: Contents -> Html ()
draw (Contents ys y ps) = with ul_ [class_ "contents-year"] $ mapM_ renderYear ys
    where
        renderYear :: Year -> Html ()
        renderYear y'
            | y == y'   = li_ (renderYearLink y' >> renderPosts)
            | otherwise = li_ (renderYearLink y')
        renderPosts = with ul_ [class_ "contents-post"] $ mapM_ renderPost ps

renderYearLink :: Year -> Html ()
renderYearLink y = with a_ [href_ url] (toHtml y)
    where
        url = "/admin/years/" <> toText y

renderPost :: Post.Post -> Html ()
renderPost p = li_ $ do
    with a_ [href_ ("/admin/posts/" <> toText (Post.postId p))] $ case Post.postTitle p of
        "" -> toHtml (timeToText $ Post.postCreated p)
        t  -> toHtml t
