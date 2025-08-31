module Madf.Blog.Contents
    ( Contents (..)
    , get
    , render
    ) where

import Database.SQLite.Simple
import Lucid
import Madf.Blog.Post.View qualified as Post
import Madf.Blog.Time
import Madf.Blog.Config
import Madf.Blog.ToText

data Contents = Contents
    { contYears   :: ![Year]
    , contCurYear :: !Year
    , contPosts   :: ![Post.Post]
    } deriving (Show)

get :: Connection -> Year -> IO Contents
get conn y = do
    ys <- Post.years conn
    ps <- Post.year conn y
    return $ Contents ys y ps

render :: Config -> Contents -> Html ()
render conf (Contents ys y ps) = with ul_ [class_ "contents-year"] $ mapM_ renderYear ys
    where
        renderYear :: Year -> Html ()
        renderYear y'
            | y == y'   = li_ (renderYearLink conf y' >> renderPosts)
            | otherwise = li_ (renderYearLink conf y')
        renderPosts = with ul_ [class_ "contents-post"] $ mapM_ (renderPost conf) ps

renderYearLink :: Config -> Year -> Html ()
renderYearLink conf y = with a_ [href_ url] (toHtml y)
    where
        url = "/" <> urlBase (main conf) <> "/" <> toText y

renderPost :: Config -> Post.Post -> Html ()
renderPost conf p = li_ $ do
    with a_ [href_ (Post.url conf p)] $ case Post.postTitle p of
        "" -> toHtml (timeToText $ Post.postCreated p)
        t  -> toHtml t
