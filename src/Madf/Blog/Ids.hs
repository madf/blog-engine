{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Madf.Blog.Ids
    ( makeId
    , fromId
    , PostId
    , ImageId
    ) where

import Data.Int (Int64)
import Data.Aeson (FromJSON, ToJSON)
import Data.Text.Lazy.Builder
import Data.Text.Lazy.Builder.Int
import Database.SQLite.Simple.FromField
import Database.SQLite.Simple.ToField
import Web.Scotty

data Entity = Post | Image

newtype Id (a :: Entity) = Id Int64 deriving (Show, Eq, FromField, ToField, FromJSON, ToJSON, Parsable)

makeId :: Int64 -> Id a
makeId = Id

fromId :: Id a -> Builder
fromId (Id v) = decimal v

type PostId = Id 'Post
type ImageId = Id 'Image
