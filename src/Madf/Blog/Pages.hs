module Madf.Blog.Pages
    ( mainPage
    , notFound
    ) where

import Data.Text
import Data.Maybe
import Lucid
import qualified Madf.Blog.Post.View as Post
import qualified Madf.Blog.Image as Image

mainPage :: [Post.Post] -> Html ()
mainPage posts = do
    ul_ $ do
        mapM_ renderExerpt posts

renderExerpt :: Post.Post -> Html ()
renderExerpt p = li_ $ do
    h4_ (toHtml $ Post.postTitle p)
    div_ (exerpt p)

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

notFound :: Text -> Html ()
notFound m = do
    h1_ "Not found"
    p_ $ toHtml m
