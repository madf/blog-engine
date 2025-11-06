module Madf.Blog.Admin.Pages.Login
    ( login
    ) where

import Data.Text
import Lucid
import Madf.Blog.Admin.Pages.Template
import Madf.Blog.Time

login :: Year -> Maybe Text -> Html ()
login y me = base y $ do
    main_ $ do
        section_ $ do
            case me of
                Just e -> with div_ [class_ "error-message visible", id_ "error-message"] $ toHtml e
                Nothing -> with div_ [class_ "error-message", id_ "error-message"] ""
            with form_ [class_ "login-form", method_ "post", id_ "login-form"] $ do
                with div_ [class_ "form-row"] $ do
                    with label_ [for_ "login"] "Login"
                    input_ [type_ "text", name_ "login", id_ "login", autofocus_]
                with div_ [class_ "form-row"] $ do
                    with label_ [for_ "password"] "Password"
                    input_ [type_ "password", name_ "password", id_ "password"]
                with div_ [class_ "form-footer"] $ do
                    with button_ [class_ "btn btn-primary", type_ "submit", id_ "submit-btn"] "Login"
    script_ [src_ "/js/login.js"] ("" :: Text)
