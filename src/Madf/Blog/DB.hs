module Madf.Blog.DB
    ( check
    ) where

import Control.Monad
import qualified Data.Text as DT
import Database.SQLite.Simple

check :: IO ()
check = do
    conn <- open "test.db"
    ts <- fmap fromOnly <$> query_ conn "SELECT name FROM sqlite_master" :: IO [DT.Text]
    unless ("posts" `elem` ts) (createPostsTable conn)

createPostsTable :: Connection -> IO ()
createPostsTable conn = execute_ conn q
    where
        q = "CREATE TABLE posts (id INTEGER PRIMARY KEY, title TEXT NOT NULL, content TEXT NOT NULL, is_draft BOOL NOT NULL, created INTEGER NOT NULL, updated INTEGER)"
