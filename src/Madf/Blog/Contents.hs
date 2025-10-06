module Madf.Blog.Contents
    ( Contents (..)
    , get
    ) where

import Database.SQLite.Simple
import Madf.Blog.Post.View qualified as Post
import Madf.Blog.Time

data Contents = Contents
    { contYears   :: ![Year]
    , contCurYear :: !Year
    , contPosts   :: ![Post.Post]
    } deriving (Show)

get :: Connection -> Year -> Int -> Int -> IO Contents
get conn y page perPage = do
    ys <- Post.years conn
    ps <- Post.year conn y page perPage
    return $ Contents ys y ps
