module Madf.Blog.DB
    ( check
    ) where

import Control.Monad
import qualified Data.Text as DT
import Database.SQLite.Simple

check :: Connection -> IO ()
check conn = do
    ts <- fmap fromOnly <$> query_ conn "SELECT name FROM sqlite_master" :: IO [DT.Text]
    unless ("info" `elem` ts) (createInfoTable conn)
    unless ("posts" `elem` ts) (createPostsTable conn)
    unless ("images" `elem` ts) (createImagesTable conn)

createInfoTable :: Connection -> IO ()
createInfoTable conn = execute_ conn q
    where
        q = "CREATE TABLE info (schema-version INTEGER NOT NULL)"

createPostsTable :: Connection -> IO ()
createPostsTable conn = execute_ conn q
    where
        q = "CREATE TABLE posts (id INTEGER PRIMARY KEY, title TEXT NOT NULL, content TEXT NOT NULL, is_draft BOOL NOT NULL, created INTEGER NOT NULL, updated INTEGER)"

createImagesTable :: Connection -> IO ()
createImagesTable conn = execute_ conn q
    where
        q = "CREATE TABLE images (id INTEGER PRIMARY KEY, post_id INTEGER NOT NULL, caption TEXT NOT NULL, file_name TEXT NOT NULL, file_size INTEGER NOT NULL, file_hash INT NOT NULL, width INTEGER NOT NULL, height INTEGER NOT NULL, mime_type TEXT NOT NULL, url TEXT NOT NULL, preview_file_name TEXT NOT NULL, preview_file_size INTEGER NOT NULL, preview_width INTEGER NOT NULL, preview_height INTEGER NOT NULL, preview_url TEXT NOT NULL, created INTEGER NOT NULL, updated INTEGER, FOREIGN KEY (post_id) REFERENCES posts (id))"
