module Madf.Blog.Post.Storage
    ( Post (..)
    , Block (..)
    , create
    , get
    , update
    , delete
    , list
    ) where

import Data.Text
import Data.Text.Encoding
import qualified Data.ByteString.Lazy as LBS
import Data.Time
import Data.Maybe
import Data.Aeson
import Data.Aeson.Types
import Database.SQLite.Simple
import Madf.Blog.Ids

data Post = Post
    { postId      :: !PostId
    , postCreated :: !UTCTime
    , postUpdated :: !(Maybe UTCTime)
    , postTitle   :: !Text
    , postContent :: ![Block]
    , postIsDraft :: !Bool
    } deriving (Show)

data Block = TextBlock !Text | CarouselBlock ![ImageId] deriving (Show)

instance ToJSON Block
    where
        toJSON (TextBlock t) = object
            [ "type"    .= ("text" :: Text)
            , "content" .= t
            ]
        toJSON (CarouselBlock is) = object
            [ "type"    .= ("carousel" :: Text)
            , "content" .= is
            ]
        toEncoding (TextBlock t) = pairs
            (  "type"    .= ("text" :: Text)
            <> "content" .= t
            )
        toEncoding (CarouselBlock is) = pairs
            (  "type"    .= ("carousel" :: Text)
            <> "content" .= is
            )

instance FromJSON Block
    where
        parseJSON = withObject "Madf.Blog.Block.Storage" $ \o -> do
            t <- o .: "type"
            c <- o .: "content"
            fromPieces t c
            where
                fromPieces :: Text -> Value -> Parser Block
                fromPieces "text" v = TextBlock <$> parseJSON v
                fromPieces "carousel" v = CarouselBlock <$> parseJSON v
                fromPieces t _ = fail $ "Parsing Madf.Blog.Block.Storage failed, unexpected block type: '" ++ unpack t ++ "'."

instance ToJSON Post
    where
        toJSON v = object
            [ "id"       .= postId v
            , "created"  .= postCreated v
            , "updated"  .= postUpdated v
            , "title"    .= postTitle v
            , "content"  .= postContent v
            , "is_draft" .= postIsDraft v
            ]
        toEncoding v = pairs
            (  "id"       .= postId v
            <> "created"  .= postCreated v
            <> "updated"  .= postUpdated v
            <> "title"    .= postTitle v
            <> "content"  .= postContent v
            <> "is_draft" .= postIsDraft v
            )

instance FromJSON Post
    where
        parseJSON = withObject "Madf.Blog.Post.Storage" $ \o -> Post
            <$> o .: "id"
            <*> o .: "created"
            <*> o .: "updated"
            <*> o .: "title"
            <*> o .: "content"
            <*> o .: "is_draft"

create :: Connection -> IO Post
create conn = do
    now <- getCurrentTime
    mpid <- fmap fromOnly . listToMaybe <$> query conn "INSERT INTO posts (title, content, is_draft, created, updated) VALUES ('', '', ?, ?, ?) RETURNING id" (True, now, now)
    case mpid of
        Nothing -> error "Cannot create post"
        Just pid -> do
            mp <- get conn pid
            case mp of
                Nothing -> error "Cannot create post"
                Just p -> return p

get :: Connection -> PostId -> IO (Maybe Post)
get conn pid = fmap makePost . listToMaybe <$> query conn "SELECT id, created, updated, title, content, is_draft FROM posts WHERE id = ?" (Only pid)

update :: Connection -> PostId -> Text -> [Block] -> Bool -> IO ()
update conn pid t bs isd = do
    now <- getCurrentTime
    execute conn "UPDATE posts SET title = ?, content = ?, is_draft = ? WHERE id = ?" (t, content, isd, pid)
    where
        content = encode bs

delete :: Connection -> PostId -> IO ()
delete conn pid = execute conn "DELETE FROM posts WHERE id = ?" (Only pid)

list :: Connection -> Int -> Int -> IO [Post]
list conn page perPage = fmap makePost <$> query conn "SELECT id, created, updated, title, content, is_draft FROM posts LIMIT ? OFFSET ?" (perPage, page * perPage)

makePost :: (PostId, UTCTime, Maybe UTCTime, Text, LBS.ByteString, Bool) -> Post
makePost (pid, created, updated, t, c, isd) = Post pid created updated t (fromMaybe dataError $ decode c) isd
    where
        dataError = [TextBlock "Data error", TextBlock (decodeUtf8 $ LBS.toStrict c)]
