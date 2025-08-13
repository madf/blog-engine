module Madf.Blog.Pages.PostType
    ( drawPostTypeField
    ) where

import Lucid
import Madf.Blog.Pages.Utils
import Madf.Blog.Post.Storage

drawPostTypeField :: Type -> Html ()
drawPostTypeField ty = do
    let (t, r) = splitType ty
    drawSelect "Type:" "type" types (Just t)
    with label_ [for_ "reason"] "Reason:"
    if ty == Public
        then input_ [id_ "reason", name_ "reason", type_ "text", value_ r, disabled_ ""]
        else input_ [id_ "reason", name_ "reason", type_ "text", value_ r]
    where
        types =
            [ OptActive "Public" "public"
            , OptActive "Unlisted" "unlisted"
            , OptActive "Private" "private"
            , OptDisabled "Unknown" "unknown"
            ]
