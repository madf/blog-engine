module Madf.Blog.Admin.Publish
    ( publish
    , regenerateAll
    ) where

import Data.Text (Text, unpack)
import Database.SQLite.Simple
import Lucid
import Madf.Blog.Admin.Pages.Preview
import Madf.Blog.Admin.Render.Template qualified as Render
import Madf.Blog.Admin.Render.Index qualified as Render
import Madf.Blog.Post.View qualified as Post
import Madf.Blog.Slug qualified as Slug
import Madf.Blog.Post.Type
import Madf.Blog.Config
import Madf.Blog.Files
import Madf.Blog.Time
import Madf.Blog.Contents qualified as Contents
import Madf.Blog.ToText
import Madf.Blog.Error

publish :: Connection -> Config -> Slug.Type -> IO ()
publish conn conf slug = do
    mp <- Post.get conn slug
    case mp of
        Nothing -> throwBlogError (PostNotFound $ "No such post: " <> Slug.unSlug slug)
        Just p -> do
            let year = timeToYear $ Post.postCreated p
            regenYearPosts conn dd year
            regenIndex conn conf dd
            regenYearIndex conn dd year
    where
        dd = destDir . main $ conf

regenIndex :: Connection -> Config -> Text -> IO ()
regenIndex conn conf dd = do
    checkCreateDir dd
    cy <- currentYear
    cnt <- getCnt conn cy
    posts <- Post.list conn 0 (numPosts $ main conf)
    renderToFile fp (Render.template ([], "Home") cy cnt $ Render.index cy (toRender posts))
    where
        fp = unpack (dd <> "/index.html")

regenYearIndex :: Connection -> Text -> Year -> IO ()
regenYearIndex conn dd year = do
    checkCreateDir yd
    cy <- currentYear
    cnt <- getCnt conn year
    posts <- Post.year conn year 0 maxBound
    let breadcrumbs = ([("Home", "/blog")], toText year)
    renderToFile fp (Render.template breadcrumbs cy cnt $ Render.yearIndex (toRender posts))
    where
        yd = dd <> "/" <> toText year
        fp = unpack (yd <> "/index.html")

regenYearPosts :: Connection -> Text -> Year -> IO ()
regenYearPosts conn dd year = do
    checkCreateDir yd
    cy <- currentYear
    cnt <- getCnt conn year
    posts <- Post.year conn year 0 maxBound
    mapM_ (publishPostInYear cy cnt) posts
    where
        yd = dd <> "/" <> toText year
        publishPostInYear cy cnt p = do
            let title = if Post.postTitle p == "" then "Untitled" else Post.postTitle p
                breadcrumbs = ([("Home", "/blog"), (toText year, "/blog/" <> toText year)], title)
                fp = unpack (yd <> "/" <> Slug.unSlug (Post.postSlug p) <> ".html")
            renderToFile fp (Render.template breadcrumbs cy cnt $ preview True p)

regenerateAll :: Connection -> Config -> IO ()
regenerateAll conn conf = do
    cy <- currentYear
    allPosts <- Post.list conn 0 maxBound
    let toRegen = filter (\p -> shouldRender (Post.postType p) && not (Post.postIsDraft p)) allPosts
    -- Regenerate all posts
    mapM_ (regenPost conn dd cy) toRegen
    -- Regenerate all year index pages
    years <- Post.years conn
    mapM_ (regenYearIndex conn dd) years
    -- Regenerate main index
    regenIndex conn conf dd
    where
        dd = destDir . main $ conf

regenPost :: Connection -> Text -> Year -> Post.Post -> IO ()
regenPost conn dd cy p = do
    let year = timeToYear $ Post.postCreated p
    checkCreateDir (dd <> "/" <> toText year)
    cnt <- getCnt conn year
    let title = if Post.postTitle p == "" then "Untitled" else Post.postTitle p
        breadcrumbs = ([("Home", "/blog"), (toText year, "/blog/" <> toText year)], title)
        fp = unpack (dd <> "/" <> toText year <> "/" <> Slug.unSlug (Post.postSlug p) <> ".html")
    renderToFile fp (Render.template breadcrumbs cy cnt $ preview True p)

toRender :: [Post.Post] -> [Post.Post]
toRender = filter (\p -> shouldList (Post.postType p) && not (Post.postIsDraft p))

getCnt :: Connection -> Year -> IO Contents.Contents
getCnt conn cy = do
    cnt <- Contents.get conn cy 0 maxBound
    return $ cnt{ Contents.contPosts = toRender (Contents.contPosts cnt) }
