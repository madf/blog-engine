module Madf.Blog.Admin.Render.Template
    ( template
    ) where

import Lucid
import Madf.Blog.Time
import Madf.Blog.Contents qualified as Contents
import Madf.Blog.Admin.Render.Contents qualified as Contents
import Madf.Blog.Admin.Nav qualified as Nav

template :: Nav.Breadcrumbs -> Year -> Contents.Contents -> Html () -> Html ()
template (path, current) y cnt b = doctypehtml_ $ do
    head_ $ do
        title_ "Madf's blog"
        link_ [rel_ "stylesheet", type_ "text/css", href_ "/css/styles.css"]
        link_ [rel_ "icon", type_ "image/x-icon", href_ "/favicon.png"]
        meta_ [charset_ "UTF-8"]
        meta_ [name_ "viewport", content_ "width=device-width, initial-scale=1.0"]
    body_ $ do
        with div_ [class_ "container"] $ do
            header_ $ do
                h1_ "Madf's blog"
            Nav.render path current
            main_ $ do
                section_ b
                aside_ $ do
                    with h4_ [class_ "contents-header"] "Contents"
                    Contents.draw cnt
            footer_ $ do
                if unYear y == 2025 then p_ "Copyright 2025 Maksym Mamontov"
                                    else p_ ("Copyright 2025-" <> toHtml y <> " Maksym Mamontov")

