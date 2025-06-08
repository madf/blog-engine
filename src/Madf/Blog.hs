module Madf.Blog
    ( mainPage
    , newPost
    , editPost
    , uploadImage
    , getImageInfo
    , updateImageInfo
    , deleteImageInfo
    ) where

import Data.Text
import Data.Maybe
import Database.SQLite.Simple
import Web.Scotty
import Lucid
import qualified Madf.Blog.Post as Post
import qualified Madf.Blog.Image as Image
import Madf.Blog.Ids

template :: Html () -> Html ()
template b = html_ $ do
    head_ $ do
        title_ "Madf's blog - Administrative interface"
    body_ $ do
        h1_ "Madf's blog - Administrative interface"
        hr_ []
        b
        hr_ []
        p_ "Copyright 2025 Maksym Mamontov"

mainPage :: [Post.Post] -> Html ()
mainPage posts = template $ do
    ul_ $ do
        mapM_ renderPost posts

renderPost :: Post.Post -> Html ()
renderPost p = li_ $ do
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
previewImage i = img_ [src_ (Image.previewUrl i), width_ (pack . show $ Image.imagePreviewWidth i), height_ (pack . show $ Image.imagePreviewHeight i), alt_ (Image.imageCaption i)]

firstImage :: [Post.Block] -> Maybe Image.Image
firstImage = listToMaybe . catMaybes . fmap extractFirstImage

extractFirstImage :: Post.Block -> Maybe Image.Image
extractFirstImage = \case
    (Post.CarouselBlock imgs) -> listToMaybe imgs
    _ -> Nothing

firstTextBlock :: [Post.Block] -> Maybe Text
firstTextBlock = listToMaybe . catMaybes . fmap extractText

extractText :: Post.Block -> Maybe Text
extractText = \case
    (Post.TextBlock t) -> Just t
    _ -> Nothing

newPost :: Html ()
newPost = template $ postForm "" [] True

editPost :: Post.Post -> Html ()
editPost p = template $ postForm (Post.postTitle p) (Post.postContent p) (Post.postIsDraft p)

postForm :: Text -> [Post.Block] -> Bool -> Html ()
postForm t c isd = form_ $ do
    form_ $ do
        input_ [hidden_ "", type_ "text", name_ "content", id_ "content"]
        with div_ [class_ "header"] $ do
            with label_ [for_ "title"] "Title"
            input_ [type_ "text", name_ "title", id_ "title", required_ "", value_ t]
        with div_ [class_ "editor"] $ do
            with div_ [id_ "blocksContainer"] $ do
                mapM_ renderBlock c
            with div_ [class_ "add-buttons"] $ do
                with button_ [class_ "btn btn-primary"] "Add paragraph"
                with button_ [class_ "btn btn-primary"] "Add carousel"
            with div_ [class_ "save-section"] $ do
                with button_ [class_ "btn btn-secondary"] "Save draft"
                with button_ [class_ "btn btn-primary"] "Publish"

renderBlock :: Post.Block -> Html ()
renderBlock b = with div_ [class_ "block"] $ do
    blockHeader
    with div_ [class_ "block-content"] $ do
        case b of
            Post.TextBlock t -> renderTextBlock t
            Post.CarouselBlock is -> renderCarousel is

renderTextBlock :: Text -> Html ()
renderTextBlock t = with div_ [class_ "text-block"] $ textarea_ $ toHtml t

renderCarousel :: [Image.Image] -> Html ()
renderCarousel is = with div_ [class_ "carousel-block"] $ do
    with div_ [class_ "image-upload"] $ do
        with label_ [for_ "upload_", class_ "upload-btn"] "📁 Upload Image"
        input_ [type_ "file", id_ "upload_", multiple_ "", accept_ "image/*"]
    case is of
        [] -> return ()
        _ -> with div_ [class_ "images-grid"] $ do
            mapM_ renderImage is

blockHeader :: Html ()
blockHeader = with div_ [class_ "block-header"] $ do
    with span_ [class_ "block-type"] "Text block"
    with div_ [class_ "block-conttrols"] $ do
        with button_ [class_ "btn btn-small btn-secondary"] "↑"
        with button_ [class_ "btn btn-small btn-secondary"] "↓"
        with button_ [class_ "btn btn-small btn-danger"] "x"

renderImage :: Image.Image -> Html ()
renderImage i = with div_ [class_ "image-item"] $ do
    img_ [src_ (Image.imageUrl i), alt_ (Image.imageCaption i), class_ "image-preview"]
    with div_ [class_ "image-caption"] $ do
        input_ [type_ "text", value_ (Image.imageCaption i)]
    with div_ [class_ "image-controls"] $ do
        small_ $ toHtml (Image.imageFileName i)
        with button_ [class_ "btn btn-small btn-danger"] "x"


uploadImage = undefined
getImageInfo = undefined
updateImageInfo = undefined
deleteImageInfo = undefined
