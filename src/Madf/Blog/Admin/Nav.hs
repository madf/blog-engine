module Madf.Blog.Admin.Nav
    ( render
    , Breadcrumb
    , Breadcrumbs
    ) where

import Data.Text (Text)
import Lucid

type Breadcrumb = (Text, Text)
type Breadcrumbs = ([Breadcrumb], Text)

render :: [Breadcrumb] -> Text -> Html ()
render [] current = nav_ $ span_ [class_ "breadcrumb-current"] (toHtml current)
render path current = nav_ $ do
    mapM_ renderCrumb path
    span_ [class_ "breadcrumb-current"] (toHtml current)
    where
        renderCrumb :: Breadcrumb -> Html ()
        renderCrumb (text, url) = do
            with a_ [href_ url, class_ "breadcrumb"] (toHtml text)
            span_ [class_ "breadcrumb-separator"] " / "
