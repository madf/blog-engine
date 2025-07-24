module Madf.Blog.Pages.Login
    ( loginPage
    ) where

import Data.Text
import Lucid

loginPage :: Maybe Text -> Html ()
loginPage me = do
    case me of
        Just e -> with div_ [class_ "error-message"] $ toHtml e
        Nothing -> return ()
    with form_ [class_ "login-form", method_ "post"] $ do
        with div_ [class_ "form-row"] $ do
            with label_ [for_ "login"] "Login"
            input_ [type_ "text", name_ "login", id_ "login"]
        with div_ [class_ "form-row"] $ do
            with label_ [for_ "password"] "Password"
            input_ [type_ "password", name_ "password", id_ "password"]
        with div_ [class_ "form-footer"] $ do
            with button_ [class_ "btn btn-primary", type_ "submit"] "Login"
