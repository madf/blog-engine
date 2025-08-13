module Madf.Blog.Post.View
    ( Post (..)
    , Block (..)
    , get
    , update
    , list
    ) where

import Data.Text
import Data.Time
import Data.Aeson
import Data.Aeson.Types
import Database.SQLite.Simple
import Madf.Blog.Ids
import qualified Madf.Blog.Post.Storage as Storage
import qualified Madf.Blog.Image as Image

data Post = Post
    { postId      :: !PostId
    , postCreated :: !UTCTime
    , postUpdated :: !(Maybe UTCTime)
    , postTitle   :: !Text
    , postContent :: ![Block]
    , postType    :: !Storage.Type
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
        parseJSON = withObject "Madf.Blog.Block.View" $ \o -> do
            t <- o .: "type"
            c <- o .: "content"
            fromPieces t c
            where
                fromPieces :: Text -> Value -> Parser Block
                fromPieces "text" v = TextBlock <$> parseJSON v
                fromPieces "carousel" v = CarouselBlock <$> parseJSON v
                fromPieces t _ = fail $ "Parsing Madf.Blog.Block.View failed, unexpected block type: '" ++ unpack t ++ "'."

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
            <> "type"     .= postType v
            <> "is_draft" .= postIsDraft v
            )

instance FromJSON Post
    where
        parseJSON = withObject "Madf.Blog.Post.View" $ \o -> Post
            <$> o .: "id"
            <*> o .: "created"
            <*> o .: "updated"
            <*> o .: "title"
            <*> o .: "content"
            <*> o .: "type"
            <*> o .: "is_draft"

get :: Connection -> PostId -> IO (Maybe Post)
get conn pid = do
    mp <- Storage.get conn pid
    case mp of
        Just p -> Just <$> fromStoragePost conn p
        Nothing -> return Nothing

update :: Connection -> PostId -> Text -> [Block] -> Storage.Type -> Bool -> IO ()
update conn pid t bs = Storage.update conn pid t (toStorageBlocks bs)

list :: Connection -> Int -> Int -> IO [Post]
list conn page perPage = do
    ps <- Storage.list conn page perPage
    mapM (fromStoragePost conn) ps

fromStorageBlocks :: Connection -> [Storage.Block] -> IO [Block]
fromStorageBlocks conn = mapM (fromStorageBlock conn)

toStorageBlocks :: [Block] -> [Storage.Block]
toStorageBlocks = Prelude.map toStorageBlock

toStorageBlock :: Block -> Storage.Block
toStorageBlock (TextBlock t) = Storage.TextBlock t
toStorageBlock (CarouselBlock is) = Storage.CarouselBlock (Prelude.map Image.imageId is)

fromStorageBlock :: Connection -> Storage.Block -> IO Block
fromStorageBlock _ (Storage.TextBlock t) = return $ TextBlock t
fromStorageBlock conn (Storage.CarouselBlock iids) = CarouselBlock <$> Image.getMultiple conn iids

fromStoragePost :: Connection -> Storage.Post -> IO Post
fromStoragePost conn p = do
    bs <- fromStorageBlocks conn (Storage.postContent p)
    return $ Post (Storage.postId p) (Storage.postCreated p) (Storage.postUpdated p) (Storage.postTitle p) bs (Storage.postType p) (Storage.postIsDraft p)
