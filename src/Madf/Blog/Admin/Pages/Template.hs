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
        link_ [rel_ "stylesheet", type_ "text/css", href_ "/css/common.css"]
        link_ [rel_ "stylesheet", type_ "text/css", href_ "/css/admin.css"]
        link_ [rel_ "stylesheet", type_ "text/css", href_ "/css/modal.css"]
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

jobModal :: Html ()
jobModal = with div_ [id_ "job-modal", class_ "modal"] $ do
    with div_ [class_ "modal-content"] $ do
        with div_ [class_ "modal-header"] $ do
            with h3_ [id_ "job-modal-title"] ""
        with div_ [class_ "modal-body"] $ do
            with div_ [class_ "progress-bar"] $ do
                with div_ [id_ "job-progress", class_ "progress-fill"] ""
            with p_ [id_ "job-status", class_ "job-status"] ""
        with div_ [class_ "modal-footer"] $ do
            with button_ [id_ "job-cancel-btn", class_ "btn btn-secondary"] "Cancel Job"
            with button_ [id_ "job-close-btn", class_ "btn btn-primary"] "Close"

template :: Nav.Breadcrumbs -> Year -> Contents.Contents -> Html () -> Html ()
template (path, current) y cnt b = base y $ do
            Nav.render path current
            main_ $ do
                section_ b
                aside_ $ do
                    with h4_ [class_ "contents-header"] "Contents"
                    Contents.draw cnt
            jobModal
            script_ [src_ "/js/admin.js", type_ "module"] ("" :: Text)
