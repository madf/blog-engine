module Madf.Blog.Publish
    ( publish
    ) where

import Data.Text
import Database.SQLite.Simple
import Lucid
import Madf.Blog.Pages.Preview
import qualified Madf.Blog.Post.View as Post
import Madf.Blog.Config
import Madf.Blog.Ids
import Madf.Blog.Utils

publish :: Connection -> Config -> PostId -> IO ()
publish conn conf i = do
    mp <- Post.get conn i
    case mp of
        Nothing -> error "No such post"
        Just p -> do
            publishPost dd p
            regenIndex conn dd
            regenContents conn dd
    where
        dd = destDir . main $ conf

publishPost :: Text -> Post.Post -> IO ()
publishPost dd p = renderToFile fp (preview True p)
    where
        year = timeYear (Post.postCreated p)
        fp = unpack (dd <> "/" <> year <> "/" <> toText (Post.postId p))

regenIndex :: Connection -> Text -> IO ()
regenIndex = undefined

regenContents :: Connection -> Text -> IO ()
regenContents = undefined
