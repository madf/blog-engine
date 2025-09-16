module Madf.Blog.Publish
    ( publish
    ) where

import Data.Text (Text, unpack)
import Database.SQLite.Simple
import Lucid
import Madf.Blog.Pages.Preview
import Madf.Blog.Pages.Index
import Madf.Blog.Pages.Template
import Madf.Blog.Post.View qualified as Post
import Madf.Blog.Slug qualified as Slug
import Madf.Blog.Config
import Madf.Blog.Files
import Madf.Blog.Time
import Madf.Blog.Contents qualified as Contents
import Madf.Blog.ToText

publish :: Connection -> Config -> Slug.Type -> IO ()
publish conn conf slug = do
    mp <- Post.get conn slug
    case mp of
        Nothing -> error "No such post"
        Just p -> do
            publishPost conn dd p
            regenIndex conn conf dd
            regenContents conn dd
    where
        dd = destDir . main $ conf

publishPost :: Connection -> Text -> Post.Post -> IO ()
publishPost conn dd p = do
    checkCreateDir pd
    cy <- currentYear
    cnt <- Contents.get conn (timeToYear $ Post.postCreated p)
    renderToFile fp (public cy cnt $ preview True p)
    where
        year = timeToYear (Post.postCreated p)
        pd = dd <> "/" <> toText year
        fp = unpack (pd <> "/" <> Slug.unSlug (Post.postSlug p) <> ".html")

regenIndex :: Connection -> Config -> Text -> IO ()
regenIndex conn conf dd = do
    checkCreateDir dd
    cy <- currentYear
    cnt <- Contents.get conn cy
    posts <- Post.list conn 0 (numPosts $ main conf)
    renderToFile fp (public cy cnt $ index posts)
    where
        fp = unpack (dd <> "/index.html")

regenContents :: Connection -> Text -> IO ()
regenContents = undefined
