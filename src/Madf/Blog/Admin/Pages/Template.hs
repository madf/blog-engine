module Madf.Blog.Admin.Pages.Template
    ( base
    , template
    ) where

import Data.Text
import Lucid
import Madf.Blog.Time

base :: Year -> Html () -> Html ()
base y b = doctypehtml_ $ do
    head_ $ do
        title_ (toHtml title)
        link_ [rel_ "stylesheet", type_ "text/css", href_ "/css/styles.css"]
        link_ [rel_ "icon", type_ "image/x-icon", href_ "/favicon.png"]
        meta_ [charset_ "UTF-8"]
        meta_ [name_ "viewport", content_ "width=device-width, initial-scale=1.0"]
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

template :: Year -> Html () -> Html ()
template y b = base y $ do
            nav_ $ do
                h3_ "Home"
            main_ $ do
                section_ b
                aside_ "Contents"
