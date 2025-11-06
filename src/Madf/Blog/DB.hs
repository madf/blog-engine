module Madf.Blog.DB
    ( check
    ) where

import Control.Monad
import Data.Maybe
import qualified Data.Text as DT
import Database.SQLite.Simple
import qualified Madf.Blog.Slug as Slug
import Madf.Blog.Ids
import Madf.Blog.Error

check :: Connection -> IO ()
check conn = do
    ts <- fmap fromOnly <$> query_ conn "SELECT name FROM sqlite_master" :: IO [DT.Text]
    unless ("info" `elem` ts) (createInfoTable conn)
    unless ("posts" `elem` ts) (createPostsTable conn)
    unless ("images" `elem` ts) (createImagesTable conn)
    msv <- fmap fromOnly . listToMaybe <$> query_ conn "SELECT schema_version FROM info"
    case msv of
        Nothing -> throwBlogError (SchemaError "Cannot query database schema version.")
        Just sv -> performUpdates conn sv

performUpdates :: Connection -> Int -> IO ()
performUpdates conn sv
    | sv < 2    = updateToV2 conn >> performUpdates conn 2
    | sv < 3    = updateToV3 conn >> performUpdates conn 3
    | sv < 4    = updateToV4 conn >> performUpdates conn 4
    | otherwise = return ()

updateToV2 :: Connection -> IO ()
updateToV2 conn = withTransaction conn $ do
    execute_ conn "ALTER TABLE posts ADD COLUMN type TEXT NOT NULL DEFAULT 'public'"
    execute_ conn "ALTER TABLE posts ADD COLUMN reason TEXT NOT NULL DEFAULT ''"
    execute_ conn "UPDATE info SET schema_version = 2"

updateToV3 :: Connection -> IO ()
updateToV3 conn = withTransaction conn $ do
    execute_ conn "ALTER TABLE posts ADD COLUMN slug TEXT DEFAULT NULL"
    is <- fmap fromOnly <$> query_ conn "SELECT id FROM posts"
    us <- mapM slugUpdate is
    executeMany conn "UPDATE posts SET slug = ? WHERE id = ?" us
    execute_ conn "UPDATE info SET schema_version = 3"
    where
        slugUpdate :: PostId -> IO (Slug.Type, PostId)
        slugUpdate i = do
            slug <- Slug.withId 8 i
            return (slug, i)

updateToV4 :: Connection -> IO ()
updateToV4 conn = withTransaction conn $ do
    execute_ conn "CREATE INDEX idx_posts_slug ON posts(slug)"
    execute_ conn "CREATE INDEX idx_posts_created ON posts(created)"
    execute_ conn "CREATE INDEX idx_images_post_id ON images(post_id)"
    execute_ conn "CREATE INDEX idx_images_file_hash ON images(file_hash)"
    execute_ conn "UPDATE info SET schema_version = 4"

createInfoTable :: Connection -> IO ()
createInfoTable conn = do
    execute_ conn q
    execute_ conn "INSERT INTO info (schema_version) VALUES (4)"
    where
        q = "CREATE TABLE info (schema_version INTEGER NOT NULL) STRICT"

createPostsTable :: Connection -> IO ()
createPostsTable conn = do
    execute_ conn q
    execute_ conn "CREATE INDEX idx_posts_slug ON posts(slug)"
    execute_ conn "CREATE INDEX idx_posts_created ON posts(created)"
    where
        q = "CREATE TABLE posts (id INTEGER PRIMARY KEY, slug TEXT, title TEXT NOT NULL, content BLOB NOT NULL, type TEXT NOT NULL, reason TEXT NOT NULL, is_draft INT NOT NULL, created TEXT NOT NULL, updated TEXT) STRICT"

createImagesTable :: Connection -> IO ()
createImagesTable conn = do
    execute_ conn q
    execute_ conn "CREATE INDEX idx_images_post_id ON images(post_id)"
    execute_ conn "CREATE INDEX idx_images_file_hash ON images(file_hash)"
    where
        q = "CREATE TABLE images (id INTEGER PRIMARY KEY, post_id INTEGER NOT NULL, caption TEXT NOT NULL, file_name TEXT NOT NULL, file_size INTEGER NOT NULL, file_hash INT NOT NULL, width INTEGER NOT NULL, height INTEGER NOT NULL, mime_type TEXT NOT NULL, url TEXT NOT NULL, preview_file_name TEXT NOT NULL, preview_file_size INTEGER NOT NULL, preview_width INTEGER NOT NULL, preview_height INTEGER NOT NULL, preview_url TEXT NOT NULL, created TEXT NOT NULL, updated TEXT, ref_count INT NOT NULL, FOREIGN KEY (post_id) REFERENCES posts (id)) STRICT"
