module Madf.Blog.Pages.Index
    ( index
    ) where

import Data.Text (Text, pack)
import Data.Maybe
import Lucid
import qualified Madf.Blog.Post.View as Post
import qualified Madf.Blog.Image as Image
import qualified Madf.Blog.Slug as Slug
import Madf.Blog.Config
import Madf.Blog.Utils

index :: Config -> [Post.Post] -> Html ()
index conf posts = do
    ul_ $ do
        mapM_ (renderExerpt conf) posts

renderExerpt :: Config -> Post.Post -> Html ()
renderExerpt conf p = li_ $ do
    h4_ $ do
        with a_ [href_ url] (toHtml $ Post.postTitle p)
    div_ $ do
        with a_ [href_ url] $ do
            div_ $ small_ (toHtml $ "Created: " <> timeToText (Post.postCreated p))
            div_ $ small_ (toHtml $ "Updated: " <> maybe "never" timeToText (Post.postUpdated p))
    div_ (exerpt p)
    where
        url = urlBase (main conf) <> timeYear (Post.postCreated p) <> Slug.unSlug (Post.postSlug p)

exerpt :: Post.Post -> Html ()
exerpt p = exerpt' (firstImage $ Post.postContent p) (firstTextBlock $ Post.postContent p)

exerpt' :: Maybe Image.Image -> Maybe Text -> Html ()
exerpt' Nothing Nothing = return ()
exerpt' (Just image) Nothing = div_ $ previewImage image
exerpt' Nothing (Just t) = p_ $ toHtml t
exerpt' (Just image) (Just t) = do
    div_ $ previewImage image
    p_ $ toHtml t

previewImage :: Image.Image -> Html ()
previewImage i = img_ [src_ (Image.imagePreviewURL i), width_ (pack . show $ Image.imagePreviewWidth i), height_ (pack . show $ Image.imagePreviewHeight i), alt_ (Image.imageCaption i)]

firstImage :: [Post.Block] -> Maybe Image.Image
firstImage = listToMaybe . mapMaybe extractFirstImage

extractFirstImage :: Post.Block -> Maybe Image.Image
extractFirstImage = \case
    (Post.CarouselBlock imgs) -> listToMaybe imgs
    _ -> Nothing

firstTextBlock :: [Post.Block] -> Maybe Text
firstTextBlock = listToMaybe . mapMaybe extractText

extractText :: Post.Block -> Maybe Text
extractText = \case
    (Post.TextBlock t) -> Just t
    _ -> Nothing
