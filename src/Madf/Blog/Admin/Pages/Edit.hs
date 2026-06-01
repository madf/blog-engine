module Madf.Blog.Admin.Pages.Edit
    ( edit
    ) where

import Data.Text (Text)
import Data.Text.Encoding (decodeUtf8)
import Data.ByteString.Lazy (toStrict)
import Data.Aeson (encode)
import Lucid
import qualified Madf.Blog.Post.View as Post
import qualified Madf.Blog.Slug as Slug
import Madf.Blog.Admin.Pages.PostType
import Madf.Blog.Time
import Madf.Blog.ToText

edit :: Post.Post -> Bool -> Int -> Int -> Html ()
edit p prescaleOn prescaleMin jpegQuality = do
    with form_ [id_ "post-form", action_ "", method_ "post"] $ do
        input_ [hidden_ "", type_ "text", name_ "post", id_ "post", (value_ . decodeUtf8 . toStrict . encode) p]
        with div_ [class_ "header"] $ do
            small_ (toHtml $ "Created: " <> timeToText (Post.postCreated p) <> ". Updated: " <> maybe "never" timeToText (Post.postUpdated p))
            with label_ [for_ "title"] "Title:"
            input_ [type_ "text", name_ "title", id_ "title", required_ "", value_ (Post.postTitle p)]
        with div_ [class_ "editor"] $ do
            with div_ [id_ "blocksContainer"] mempty
            with div_ [class_ "add-buttons"] $ do
                with button_ [id_ "add-text-block-button", class_ "btn btn-primary", type_ "button"] "Add paragraph"
                with button_ [id_ "add-carousel-block-button", class_ "btn btn-primary", type_ "button"] "Add carousel"
        with div_ [class_ "post-form-footer"] $ do
            drawPostTypeField (Post.postType p)
            with label_ [for_ "is_draft"] "Draft:"
            if Post.postIsDraft p then input_ [name_ "is_draft", id_ "is_draft", type_ "checkbox", checked_]
                                  else input_ [name_ "is_draft", id_ "is_draft", type_ "checkbox"]
            with a_ [id_ "preview-button", class_ "link-btn btn-secondary m-l-auto", href_ ("/admin/posts/" <> Slug.unSlug (Post.postSlug p))] "Preview"
            with button_ [id_ "save-button", class_ "btn btn-primary", type_ "button"] "Save"
    input_ [type_ "hidden", id_ "prescale-enabled",   value_ (if prescaleOn then "true" else "false")]
    input_ [type_ "hidden", id_ "prescale-threshold", value_ (toText prescaleMin)]
    input_ [type_ "hidden", id_ "jpeg-quality",       value_ (toText jpegQuality)]
    script_ [src_ "/js/edit.js", type_ "module"] ("" :: Text)
