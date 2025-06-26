module Madf.Blog.Pages
    ( mainPage
    , newPost
    , editPost
    , previewPost
    , notFound
    , badRequest
    ) where

import Data.Text
import Data.Text.Encoding (decodeUtf8)
import Data.ByteString.Lazy (toStrict)
import Data.Maybe
import Data.Aeson (encode)
import Lucid
import qualified Madf.Blog.Post as Post
import qualified Madf.Blog.Image as Image
import Madf.Blog.Utils

template :: Html () -> Html ()
template b = doctypehtml_ $ do
    head_ $ do
        title_ "Madf's blog - Administrative interface"
        link_ [rel_ "stylesheet", type_ "text/css", href_ "/css/styles.css"]
        meta_ [charset_ "UTF-8"]
        meta_ [name_ "viewport", content_ "width=device-width, initial-scale=1.0"]
    body_ $ do
        h1_ "Madf's blog - Administrative interface"
        hr_ []
        b
        hr_ []
        p_ "Copyright 2025 Maksym Mamontov"

mainPage :: [Post.Post] -> Html ()
mainPage posts = template $ do
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
previewImage i = img_ [src_ (Image.previewUrl i), width_ (pack . show $ Image.imagePreviewWidth i), height_ (pack . show $ Image.imagePreviewHeight i), alt_ (Image.imageCaption i)]

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

newPost :: Html ()
newPost = template $ postForm "" [] True

editPost :: Post.Post -> Html ()
editPost p = template $ postForm (Post.postTitle p) (Post.postContent p) (Post.postIsDraft p)

postForm :: Text -> [Post.Block] -> Bool -> Html ()
postForm t c isd = do
    with form_ [id_ "post-form", action_ "", method_ "post"] $ do
        input_ [hidden_ "", type_ "text", name_ "content", id_ "content", (value_ . decodeUtf8 . toStrict . encode) c]
        with div_ [class_ "header"] $ do
            with label_ [for_ "title"] "Title:"
            input_ [type_ "text", name_ "title", id_ "title", required_ "", value_ t]
        with div_ [class_ "editor"] $ do
            with div_ [id_ "blocksContainer"] mempty
            with div_ [class_ "add-buttons"] $ do
                with button_ [id_ "add-text-block-button", class_ "btn btn-primary", type_ "button"] "Add paragraph"
                with button_ [id_ "add-carousel-block-button", class_ "btn btn-primary", type_ "button"] "Add carousel"
        with div_ [class_ "save-section"] $ do
            with label_ [for_ "is_draft"] "Draft:"
            if isd then input_ [name_ "id_draft", id_ "is_draft", type_ "checkbox", checked_]
                   else input_ [name_ "id_draft", id_ "is_draft", type_ "checkbox"]
            with button_ [id_ "save-button", class_ "btn btn-primary", type_ "button"] "Save"
    with (script_ "") [src_ "/js/edit.js"]

previewPost :: Post.Post -> Html ()
previewPost p = template $ do
    h2_ $ toHtml (Post.postTitle p)
    div_ $ do
        small_ (toHtml $ "Created: " <> timeToText (Post.postCreated p) <> ". Updated: " <> (fromMaybe "never" $ timeToText <$> Post.postUpdated p))
    hr_ []
    mapM_ renderBlock (Post.postContent p)

renderBlock :: Post.Block -> Html ()
renderBlock = \case
    Post.TextBlock t -> p_ $ toHtml t
    Post.CarouselBlock is -> renderCarousel is

renderCarousel :: [Image.Image] -> Html ()
renderCarousel is = do
    with div_ [class_ "images-grid"] $ do
        mapM_ renderImage is

renderImage :: Image.Image -> Html ()
renderImage i = div_ $ do
    with a_ [href_ (Image.imageUrl i)] $ do
        img_ [src_ (Image.imagePreviewUrl i), alt_ (Image.imageCaption i), class_ "image-preview"]
    p_ $ toHtml (Image.imageCaption i)

notFound :: Text -> Html ()
notFound m = template $ do
    h1_ "Not found"
    p_ $ toHtml m

badRequest :: Text -> Html ()
badRequest m = template $ do
    h1_ "Bad request"
    p_ $ toHtml m
