module Madf.Blog.Pages
    ( mainPage
    , editPost
    , notFound
    , badRequest
    ) where

import Data.Text
import Data.Text.Encoding (decodeUtf8)
import Data.ByteString.Lazy (toStrict)
import Data.Maybe
import Data.Aeson (encode)
import Lucid
import qualified Madf.Blog.Post.View as Post
import qualified Madf.Blog.Image as Image
import Madf.Blog.Utils

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

editPost :: Post.Post -> Html ()
editPost = postForm

postForm :: Post.Post -> Html ()
postForm p = do
    with form_ [id_ "post-form", action_ "", method_ "post"] $ do
        input_ [hidden_ "", type_ "text", name_ "post", id_ "post", (value_ . decodeUtf8 . toStrict . encode) p]
        with div_ [class_ "header"] $ do
            div_ $ small_ (toHtml $ "Created: " <> timeToText (Post.postCreated p) <> ". Updated: " <> maybe "never" timeToText (Post.postUpdated p))
            with label_ [for_ "title"] "Title:"
            input_ [type_ "text", name_ "title", id_ "title", required_ "", value_ (Post.postTitle p)]
        with div_ [class_ "editor"] $ do
            with div_ [id_ "blocksContainer"] mempty
            with div_ [class_ "add-buttons"] $ do
                with button_ [id_ "add-text-block-button", class_ "btn btn-primary", type_ "button"] "Add paragraph"
                with button_ [id_ "add-carousel-block-button", class_ "btn btn-primary", type_ "button"] "Add carousel"
        with div_ [class_ "save-section"] $ do
            with label_ [for_ "is_draft"] "Draft:"
            if Post.postIsDraft p then input_ [name_ "id_draft", id_ "is_draft", type_ "checkbox", checked_]
                                  else input_ [name_ "id_draft", id_ "is_draft", type_ "checkbox"]
            with button_ [id_ "save-button", class_ "btn btn-primary", type_ "button"] "Save"
    with (script_ "") [src_ "/js/edit.js"]

notFound :: Text -> Html ()
notFound m = do
    h1_ "Not found"
    p_ $ toHtml m

badRequest :: Text -> Html ()
badRequest m = do
    h1_ "Bad request"
    p_ $ toHtml m
