module Madf.Blog.Admin.Pages.Index
    ( index
    ) where

import Data.Text (Text, pack)
import Data.Maybe
import Lucid
import qualified Madf.Blog.Post.View as Post
import qualified Madf.Blog.Image as Image
import qualified Madf.Blog.Slug as Slug
import Madf.Blog.Time

index :: [Post.Post] -> Html ()
index posts = do
    div_ [class_ "add-buttons"] $ do
        with button_ [class_ "btn btn-primary", id_ "regenerate-all-btn"] "Regenerate All Pages"
    mapM_ renderExcerpt posts

renderExcerpt :: Post.Post -> Html ()
renderExcerpt p = with div_ [class_ "excerpt"] $ do
    with a_ [href_ url, class_ "post-header"] $ do
        with div_ [class_ "date-box"] $ do
            let (m, d) = splitDate (Post.postCreated p)
            span_ $ toHtml m
            span_ $ toHtml d
        h2_ $ toHtml (Post.postTitle p)
    excerpt p
    with div_ [class_ "post-footer"] $ do
        with a_ [href_ url, class_ "btn btn-secondary btn-small"] "Edit"
    where
        url = "/admin/posts/" <> Slug.unSlug (Post.postSlug p)

excerpt :: Post.Post -> Html ()
excerpt p = div_ $ excerpt' (firstImage $ Post.postContent p) (firstTextBlock $ Post.postContent p)

excerpt' :: Maybe Image.Image -> Maybe Text -> Html ()
excerpt' Nothing Nothing = return ()
excerpt' (Just image) Nothing = div_ $ previewImage image
excerpt' Nothing (Just t) = p_ $ toHtml t
excerpt' (Just image) (Just t) = do
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
