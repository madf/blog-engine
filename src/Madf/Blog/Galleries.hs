module Madf.Blog.Galleries
    ( list
    , Post.pages
    , Gallery (..)
    ) where

import Data.Text
import Data.Maybe
import Data.Aeson
import Database.SQLite.Simple
import Madf.Blog.Image (Image)
import Madf.Blog.Image qualified as Image
import Madf.Blog.Post.Storage qualified as Post
import Madf.Blog.Ids

data Gallery = Gallery
    { galPostId    :: !PostId
    , galPostTitle :: !Text
    , galImages    :: ![Image]
    } deriving (Show)

instance ToJSON Gallery
    where
        toJSON v = object
            [ "id"     .= galPostId v
            , "title"  .= galPostTitle v
            , "images" .= galImages v
            ]
        toEncoding v = pairs
            (  "id"     .= galPostId v
            <> "title"  .= galPostTitle v
            <> "images" .= galImages v
            )

list :: Connection -> Int -> Int -> IO [Gallery]
list conn page perPage = do
    pinfos <- query conn "SELECT id, title FROM posts ORDER BY created DESC LIMIT ? OFFSET ?" (perPage, page * perPage)
    mapM getGal pinfos
    where
        getGal (pid, t) = do
            imgs <- Image.listByPost conn pid
            return $ Gallery pid t imgs
