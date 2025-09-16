module Madf.Blog.Post.View
    ( Post (..)
    , Block (..)
    , get
    , update
    , list
    , years
    , year
    , url
    ) where

import Data.Text
import Data.Time.Clock
import Data.Aeson
import Data.Aeson.Types
import Database.SQLite.Simple
import Madf.Blog.Ids
import Madf.Blog.ToText
import Madf.Blog.Time
import qualified Madf.Blog.Post.Storage as Storage
import qualified Madf.Blog.Image as Image
import qualified Madf.Blog.Slug as Slug

data Post = Post
    { postId      :: !PostId
    , postSlug    :: !Slug.Type
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
            , "slug"     .= postSlug v
            , "created"  .= postCreated v
            , "updated"  .= postUpdated v
            , "title"    .= postTitle v
            , "content"  .= postContent v
            , "is_draft" .= postIsDraft v
            ]
        toEncoding v = pairs
            (  "id"       .= postId v
            <> "slug"     .= postSlug v
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
            <*> o .: "slug"
            <*> o .: "created"
            <*> o .: "updated"
            <*> o .: "title"
            <*> o .: "content"
            <*> o .: "type"
            <*> o .: "is_draft"

get :: Connection -> Slug.Type -> IO (Maybe Post)
get conn slug = do
    mp <- Storage.get conn slug
    case mp of
        Just p -> Just <$> fromStoragePost conn p
        Nothing -> return Nothing

update :: Connection -> Slug.Type -> Text -> [Block] -> Storage.Type -> Bool -> IO ()
update conn slug t bs = Storage.update conn slug t (toStorageBlocks bs)

list :: Connection -> Int -> Int -> IO [Post]
list conn page perPage = do
    ps <- Storage.list conn page perPage
    mapM (fromStoragePost conn) ps

years :: Connection -> IO [Year]
years = Storage.years

year :: Connection -> Year -> IO [Post]
year conn y = do
    ps <- Storage.year conn y
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
    return $ Post (Storage.postId p) (Storage.postSlug p) (Storage.postCreated p) (Storage.postUpdated p) (Storage.postTitle p) bs (Storage.postType p) (Storage.postIsDraft p)

url :: Post -> Text
url p = "/blog/" <> toText (timeToYear $ postCreated p) <> "/" <> Slug.unSlug (postSlug p)
