module Madf.Blog.Post.Storage
    ( Post (..)
    , Type (..)
    , Block (..)
    , makeType
    , splitType
    , create
    , get
    , update
    , list
    , years
    , year
    , pages
    ) where

import Data.List.NonEmpty qualified as DLN
import Data.Text
import Data.Text.Encoding
import Data.ByteString.Lazy qualified as LBS
import Data.Time.Clock
import Data.Maybe
import Data.Aeson
import Data.Aeson.Types
import Database.SQLite.Simple
import Madf.Blog.Slug qualified as Slug
import Madf.Blog.Post.Type
import Madf.Blog.Time
import Madf.Blog.Ids
import Madf.Blog.Error

data Post = Post
    { postId      :: !PostId
    , postSlug    :: !Slug.Type
    , postCreated :: !UTCTime
    , postUpdated :: !(Maybe UTCTime)
    , postTitle   :: !Text
    , postContent :: ![Block]
    , postType    :: !Type
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
        parseJSON = withObject "Madf.Blog.Post.Block.Storage" $ \o -> do
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
            , "slug"     .= postSlug v
            , "created"  .= postCreated v
            , "updated"  .= postUpdated v
            , "title"    .= postTitle v
            , "content"  .= postContent v
            , "type"     .= postType v
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
        parseJSON = withObject "Madf.Blog.Post.Storage" $ \o -> Post
            <$> o .: "id"
            <*> o .: "slug"
            <*> o .: "created"
            <*> o .: "updated"
            <*> o .: "title"
            <*> o .: "type"
            <*> o .: "content"
            <*> o .: "is_draft"

create :: Connection -> IO Post
create conn = do
    now <- getCurrentTime
    mpid <- fmap fromOnly . listToMaybe <$> query conn "INSERT INTO posts (slug, title, content, type, reason, is_draft, created, updated) VALUES (NULL, '', ?, 'unlisted', '', ?, ?, ?) RETURNING id" ("[]" :: LBS.ByteString, True, now, now)
    case mpid of
        Nothing -> throwBlogError (DatabaseError "Failed to insert post into database")
        Just pid -> do
            slug <- Slug.withId 8 pid
            execute conn "UPDATE posts SET slug = ? WHERE id = ?" (slug, pid)
            mp <- getById conn pid
            case mp of
                Nothing -> throwBlogError (PostNotFound "Failed to retrieve newly created post")
                Just p -> return p

selectBase :: Query
selectBase = "SELECT id, slug, created, updated, title, content, type, reason, is_draft FROM posts"

get :: Connection -> Slug.Type -> IO (Maybe Post)
get conn slug = fmap makePost . listToMaybe <$> query conn (selectBase <> " WHERE slug = ?") (Only slug)

getById :: Connection -> PostId -> IO (Maybe Post)
getById conn pid = fmap makePost . listToMaybe <$> query conn (selectBase <> " WHERE id = ?") (Only pid)

update :: Connection -> Slug.Type -> Text -> [Block] -> Type -> Bool -> IO ()
update conn slug ti bs ty isd = do
    now <- getCurrentTime
    let (t, r) = splitType ty
    execute conn "UPDATE posts SET title = ?, content = ?, type = ?, reason = ?, is_draft = ?, updated = ? WHERE slug = ?" (ti, content, t, r, isd, now, slug)
    where
        content = encode bs

list :: Connection -> Int -> Int -> IO [Post]
list conn page perPage = fmap makePost <$> query conn (selectBase <> " ORDER BY created DESC LIMIT ? OFFSET ?") (perPage, page * perPage)

years :: Connection -> IO [Year]
years conn = fmap DLN.head . DLN.group . fmap (timeToYear . fromOnly) <$> query_ conn q
    where
        q = "SELECT created FROM posts ORDER BY created DESC"

year :: Connection -> Year -> Int -> Int -> IO [Post]
year conn y page perPage = fmap makePost <$> query conn (selectBase <> " WHERE created BETWEEN ? AND ? ORDER BY created DESC LIMIT ? OFFSET ?") (yearStart y, yearStart (succ y), perPage, page * perPage)

makePost :: (PostId, Slug.Type, UTCTime, Maybe UTCTime, Text, LBS.ByteString, Text, Text, Bool) -> Post
makePost (pid, slug, created, updated, t, c, ty, r, isd) = Post pid slug created updated t (fromMaybe dataError $ decode c) (makeType ty r) isd
    where
        dataError = [TextBlock "Data error", TextBlock (decodeUtf8 $ LBS.toStrict c)]

pages :: Connection -> Int -> IO Int
pages conn perPage = do
    postNum <- (fromMaybe 0 . listToMaybe . fmap fromOnly) <$> query_ conn "SELECT COUNT(*) FROM posts"
    return $ (postNum - 1) `div` perPage + 1
