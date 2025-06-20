module Madf.Blog.Post
    ( Post (..)
    , Block (..)
    , create
    , get
    , update
    , delete
    , publish
    , hide
    , list
    ) where

import Data.Text
import Data.Time
import Data.Aeson
import Data.Aeson.Types
import Database.SQLite.Simple
import Madf.Blog.Ids
import qualified Madf.Blog.Image as Image

data Post = Post
    { postId      :: !PostId
    , postCreated :: !UTCTime
    , postUpdated :: !UTCTime
    , postTitle   :: !Text
    , postContent :: ![Block]
    , postIsDraft :: !Bool
    } deriving (Show)

data Block = TextBlock !Text | CarouselBlock ![Image.Image] deriving (Show)

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
        parseJSON = withObject "Madf.Blog.Block" $ \o -> do
            t <- o .: "type"
            c <- o .: "content"
            fromPieces t c
            where
                fromPieces :: Text -> Value -> Parser Block
                fromPieces "text" v = TextBlock <$> parseJSON v
                fromPieces "carousel" v = CarouselBlock <$> parseJSON v
                fromPieces t _ = fail $ "Parsing Madf.Blog.Block failed, unexpected block type: '" ++ unpack t ++ "'."

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
        parseJSON = withObject "Madf.Blog.Post" $ \o -> Post
            <$> o .: "id"
            <*> o .: "created"
            <*> o .: "updated"
            <*> o .: "title"
            <*> o .: "content"
            <*> o .: "is_draft"

create :: Connection -> Text -> [Block] -> IO ()
create conn t bs = do
    now <- getCurrentTime
    void $ execute conn "INSERT INTO posts (title, content, is_draft, created, updated) VALUES (?, ?, ?, ?, ?)" (t, content, True, now, now)
    where
        content = encode bs

get :: Connection -> PostId -> IO (Maybe Post)
get conn pid = fmap makePost . listToMaybe <$> query conn "SELECT id, created, updated, title, content, is_draft" (Only pid)

update :: Connection -> PostId -> Text -> [Block] -> Bool -> IO ()
update conn pid t bs isd = do
    now <- getCurrentTime
    void $ execute conn "UPDATE posts SET title = ?, content = ?, is_draft = ? WHERE id = ?" (t, content, isd, pid)
    where
        content = encode bs

delete :: Connection -> PostId -> IO ()
delete conn pid = void $ execute conn "DELETE FROM posts WHERE id = ?" (Only pid)

list :: Connection -> Int -> Int -> IO [Post]
list conn page perPage = fmap makePost <$> query conn "SELECT id, created, updated, title, content, is_draft LIMIT ? OFFSET ?" (perPage, page * perPage)

makePost :: (PostId, UTCTime, UTCTime, Text, ByteString, Bool) -> Post
makePost (pid, created, updated, t, c, isd) = Post
