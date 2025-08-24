module Madf.Blog.Publish
    ( publish
    ) where

import Data.Text (Text, unpack)
import Database.SQLite.Simple
import Lucid
import Madf.Blog.Pages.Preview
import Madf.Blog.Pages.Index
import Madf.Blog.Pages.Template
import qualified Madf.Blog.Post.View as Post
import qualified Madf.Blog.Slug as Slug
import Madf.Blog.Config
import Madf.Blog.Files
import Madf.Blog.Utils

publish :: Connection -> Config -> Slug.Type -> IO ()
publish conn conf slug = do
    mp <- Post.get conn slug
    case mp of
        Nothing -> error "No such post"
        Just p -> do
            publishPost dd p
            regenIndex conn conf dd
            regenContents conn dd
    where
        dd = destDir . main $ conf

publishPost :: Text -> Post.Post -> IO ()
publishPost dd p = do
    checkCreateDir pd
    cy <- currentYear
    renderToFile fp (public cy $ preview True p)
    where
        year = timeYear (Post.postCreated p)
        pd = dd <> "/" <> year
        fp = unpack (pd <> "/" <> Slug.unSlug (Post.postSlug p) <> ".html")

regenIndex :: Connection -> Config -> Text -> IO ()
regenIndex conn conf dd = do
    checkCreateDir dd
    cy <- currentYear
    posts <- Post.list conn 0 (numPosts $ main conf)
    renderToFile fp (public cy $ index conf posts)
    where
        fp = unpack (dd <> "/index.html")

regenContents :: Connection -> Text -> IO ()
regenContents = undefined
