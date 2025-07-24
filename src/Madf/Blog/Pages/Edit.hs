module Madf.Blog.Pages.Edit
    ( editPage
    ) where

import Data.Text.Encoding (decodeUtf8)
import Data.ByteString.Lazy (toStrict)
import Data.Aeson (encode)
import Lucid
import qualified Madf.Blog.Post.View as Post
import Madf.Blog.Utils

editPage :: Post.Post -> Html ()
editPage p = do
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
