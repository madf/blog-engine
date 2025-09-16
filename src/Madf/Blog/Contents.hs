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

get :: Connection -> Year -> IO Contents
get conn y = do
    ys <- Post.years conn
    ps <- Post.year conn y
    return $ Contents ys y ps
