module Madf.Blog.Pages.Template
    ( template
    ) where

import Data.Text
import Lucid

template :: Text -> Html () -> Html ()
template y b = doctypehtml_ $ do
    head_ $ do
        title_ "Madf's blog - Administrative interface"
        link_ [rel_ "stylesheet", type_ "text/css", href_ "/css/styles.css"]
        meta_ [charset_ "UTF-8"]
        meta_ [name_ "viewport", content_ "width=device-width, initial-scale=1.0"]
    body_ $ do
        with div_ [class_ "container"] $ do
            header_ $ do
                h1_ "Madf's blog - Administrative interface"
                hr_ []
            main_ b
            footer_ $ do
                hr_ []
                if y == "2025" then p_ "Copyright 2025 Maksym Mamontov"
                               else p_ ("Copyright 2025-" <> toHtml y <> " Maksym Mamontov")
