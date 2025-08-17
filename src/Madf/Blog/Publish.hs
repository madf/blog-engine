module Madf.Blog.Publish
    ( publish
    ) where

import Data.Text
import Database.SQLite.Simple
import Lucid
import Madf.Blog.Pages.Preview
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
            regenIndex conn dd
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

regenIndex :: Connection -> Text -> IO ()
regenIndex = undefined

regenContents :: Connection -> Text -> IO ()
regenContents = undefined
