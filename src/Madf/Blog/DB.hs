module Madf.Blog.DB
    ( check
    ) where

import Control.Monad
import Data.Maybe
import qualified Data.Text as DT
import Database.SQLite.Simple

check :: Connection -> IO ()
check conn = do
    ts <- fmap fromOnly <$> query_ conn "SELECT name FROM sqlite_master" :: IO [DT.Text]
    unless ("info" `elem` ts) (createInfoTable conn)
    unless ("posts" `elem` ts) (createPostsTable conn)
    unless ("images" `elem` ts) (createImagesTable conn)
    msv <- fmap fromOnly . listToMaybe <$> query_ conn "SELECT schema_version FROM info"
    case msv of
        Nothing -> error "Cannot query database schema version."
        Just sv -> performUpdates conn sv

performUpdates :: Connection -> Int -> IO ()
performUpdates conn sv
    | sv < 2    = updateToV2 conn
    | otherwise = return ()

updateToV2 :: Connection -> IO ()
updateToV2 conn = withTransaction conn $ do
    execute_ conn "ALTER TABLE posts ADD COLUMN type TEXT NOT NULL DEFAULT 'public'"
    execute_ conn "ALTER TABLE posts ADD COLUMN reason TEXT NOT NULL DEFAULT ''"
    execute_ conn "UPDATE info SET schema_version = 2"

createInfoTable :: Connection -> IO ()
createInfoTable conn = do
    execute_ conn q
    execute_ conn "INSERT INTO info (schema_version) VALUES (1)"
    where
        q = "CREATE TABLE info (schema_version INTEGER NOT NULL) STRICT"

createPostsTable :: Connection -> IO ()
createPostsTable conn = execute_ conn q
    where
        q = "CREATE TABLE posts (id INTEGER PRIMARY KEY, title TEXT NOT NULL, content BLOB NOT NULL, is_draft INT NOT NULL, created TEXT NOT NULL, updated TEXT) STRICT"

createImagesTable :: Connection -> IO ()
createImagesTable conn = execute_ conn q
    where
        q = "CREATE TABLE images (id INTEGER PRIMARY KEY, post_id INTEGER NOT NULL, caption TEXT NOT NULL, file_name TEXT NOT NULL, file_size INTEGER NOT NULL, file_hash INT NOT NULL, width INTEGER NOT NULL, height INTEGER NOT NULL, mime_type TEXT NOT NULL, url TEXT NOT NULL, preview_file_name TEXT NOT NULL, preview_file_size INTEGER NOT NULL, preview_width INTEGER NOT NULL, preview_height INTEGER NOT NULL, preview_url TEXT NOT NULL, created TEXT NOT NULL, updated TEXT, ref_count INT NOT NULL, FOREIGN KEY (post_id) REFERENCES posts (id)) STRICT"
