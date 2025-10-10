module Madf.Blog.Admin.Pages.Template
    ( base
    , template
    ) where

import Data.Text
import Lucid
import Madf.Blog.Time
import Madf.Blog.Admin.Nav qualified as Nav
import Madf.Blog.Contents qualified as Contents
import Madf.Blog.Admin.Pages.Contents qualified as Contents

base :: Year -> Html () -> Html ()
base y b = doctypehtml_ $ do
    head_ $ do
        title_ (toHtml title)
        link_ [rel_ "stylesheet", type_ "text/css", href_ "/css/styles.css"]
        link_ [rel_ "icon", type_ "image/x-icon", href_ "/favicon.png"]
        meta_ [charset_ "UTF-8"]
        meta_ [name_ "viewport", content_ "width=device-width, initial-scale=1.0"]
        script_ [src_ "/js/admin.js"] ("" :: Text)
    body_ $ do
        with div_ [class_ "container"] $ do
            header_ $ do
                h1_ (toHtml title)
            b
            footer_ $ do
                if unYear y == 2025 then p_ "Copyright 2025 Maksym Mamontov"
                                    else p_ ("Copyright 2025-" <> toHtml y <> " Maksym Mamontov")
    where
        title :: Text
        title = "Madf's blog - Administrative interface"

template :: Nav.Breadcrumbs -> Year -> Contents.Contents -> Html () -> Html ()
template (path, current) y cnt b = base y $ do
            Nav.render path current
            main_ $ do
                section_ b
                aside_ $ do
                    with h4_ [class_ "contents-header"] "Contents"
                    Contents.draw cnt
