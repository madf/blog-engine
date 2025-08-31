module Madf.Blog.Pages
    ( mainPage
    , notFound
    ) where

import Data.Text
import Data.Maybe
import Lucid
import qualified Madf.Blog.Post.View as Post
import qualified Madf.Blog.Image as Image
import qualified Madf.Blog.Slug as Slug
import Madf.Blog.Time

mainPage :: [Post.Post] -> Html ()
mainPage posts = do
    ul_ $ do
        mapM_ renderExcerpt posts

renderExcerpt :: Post.Post -> Html ()
renderExcerpt p = with li_ [class_ "excerpt"] $ do
    h4_ $ do
        with a_ [href_ url] (toHtml $ Post.postTitle p)
    div_ $ do
        with a_ [href_ url] $ do
            div_ $ small_ (toHtml $ "Created: " <> timeToText (Post.postCreated p))
            div_ $ small_ (toHtml $ "Updated: " <> maybe "never" timeToText (Post.postUpdated p))
    div_ (excerpt p)
    where
        url = "/admin/preview/" <> Slug.unSlug (Post.postSlug p)

excerpt :: Post.Post -> Html ()
excerpt p = excerpt' (firstImage $ Post.postContent p) (firstTextBlock $ Post.postContent p)

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

notFound :: Text -> Html ()
notFound m = do
    h1_ "Not found"
    p_ $ toHtml m
