module Madf.Blog.Pages.Preview
    ( preview
    ) where

import Control.Monad
import Data.Text
import Lucid
import qualified Madf.Blog.Post.View as Post
import qualified Madf.Blog.Image as Image
import Madf.Blog.Ids
import Madf.Blog.Utils

preview :: Bool -> Post.Post -> Html ()
preview r p = do
    with div_ [class_ "post-header"] $ do
        with div_ [class_ "date-box"] $ do
            let (m, d) = splitDate (Post.postCreated p)
            span_ $ toHtml m
            span_ $ toHtml d
        h2_ $ toHtml (Post.postTitle p)
        unless r $ with a_ [href_ ("/admin/edit/" <> toText (Post.postId p)), class_ "link-btn btn-secondary m-l-auto"] (span_ [class_ "icon"] "✎")
    hr_ []
    mapM_ renderBlock (Prelude.zip (Post.postContent p) [1..])
    div_ [class_ "post-footer"] $ do
        small_ (toHtml $ "Created: " <> timeToText (Post.postCreated p) <> ". Updated: " <> maybe "never" timeToText (Post.postUpdated p))
    with (script_ "") [src_ "/js/carousel.js"]

renderBlock :: (Post.Block, Int) -> Html ()
renderBlock = \case
    (Post.TextBlock t, _) -> with p_ [class_ "text-block-preview"] $ toHtml t
    (Post.CarouselBlock is, idx) -> renderCarousel idx is

renderCarousel :: Int -> [Image.Image] -> Html ()
renderCarousel idx is = do
    let cid = pack (show idx)
    with div_ [class_ "carousel-preview-block", id_ ("carousel-preview-" <> cid)] $ do
        with div_ [class_ "carousel-preview"] $ do
            mapM_ renderImage (Prelude.zip is [0..])
        with a_ [class_ "carousel-btn-prev"] "❮"
        with a_ [class_ "carousel-btn-next"] "❯"
        with div_ [class_ "carousel-dots", id_ ("carousel-dots-" <> cid)] $ do
            mapM_ (\cn -> with span_ [class_ cn] mempty) ("carousel-dot carousel-dot-active":Prelude.replicate (Prelude.length is - 1) "carousel-dot")

renderImage :: (Image.Image, Int) -> Html ()
renderImage (i, idx) = with div_ [class_ cn] $ do
    with a_ [href_ (Image.imageURL i)] $ do
        img_ [src_ (Image.imagePreviewURL i), alt_ (Image.imageCaption i), class_ "carousel-image", width_ (pack . show . Image.imagePreviewWidth $ i), height_ (pack . show . Image.imagePreviewHeight $ i)]
    with p_ [class_ "carousel-image-caption"] $ toHtml (Image.imageCaption i)
    where
        cn = if idx == 0 then "carousel-image-show carousel-fade"
                         else "carousel-image-hide carousel-fade"
