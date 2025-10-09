module Madf.Blog.Admin.Publish
    ( publish
    ) where

import Data.Text (Text, unpack)
import Database.SQLite.Simple
import Lucid
import Madf.Blog.Admin.Pages.Preview
import Madf.Blog.Admin.Render.Template qualified as Render
import Madf.Blog.Admin.Render.Index qualified as Render
import Madf.Blog.Post.View qualified as Post
import Madf.Blog.Slug qualified as Slug
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
            publishPost conn dd p
            regenIndex conn conf dd
            regenYearIndex conn dd (timeToYear $ Post.postCreated p)
            regenContents conn dd
    where
        dd = destDir . main $ conf

publishPost :: Connection -> Text -> Post.Post -> IO ()
publishPost conn dd p = do
    checkCreateDir pd
    cy <- currentYear
    cnt <- Contents.get conn (timeToYear $ Post.postCreated p) 0 maxBound
    renderToFile fp (Render.template cy cnt $ preview True p)
    where
        year = timeToYear (Post.postCreated p)
        pd = dd <> "/" <> toText year
        fp = unpack (pd <> "/" <> Slug.unSlug (Post.postSlug p) <> ".html")

regenIndex :: Connection -> Config -> Text -> IO ()
regenIndex conn conf dd = do
    checkCreateDir dd
    cy <- currentYear
    cnt <- Contents.get conn cy 0 maxBound
    posts <- Post.list conn 0 (numPosts $ main conf)
    renderToFile fp (Render.template cy cnt $ Render.index cy posts)
    where
        fp = unpack (dd <> "/index.html")

regenYearIndex :: Connection -> Text -> Year -> IO ()
regenYearIndex conn dd year = do
    checkCreateDir yd
    cy <- currentYear
    cnt <- Contents.get conn year 0 maxBound
    posts <- Post.year conn year 0 maxBound
    renderToFile fp (Render.template cy cnt $ Render.yearIndex posts)
    where
        yd = dd <> "/" <> toText year
        fp = unpack (yd <> "/index.html")

regenContents :: Connection -> Text -> IO ()
regenContents = undefined
