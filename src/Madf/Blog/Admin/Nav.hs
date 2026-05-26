module Madf.Blog.Admin.Nav
    ( render
    , renderSidebar
    , Breadcrumb
    , Breadcrumbs
    , Section(..)
    ) where

import Data.Text (Text)
import Lucid

type Breadcrumb = (Text, Text)
type Breadcrumbs = ([Breadcrumb], Text)

data Section = Posts | Galleries | Jobs deriving (Eq)

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

renderSidebar :: Section -> Html ()
renderSidebar active = with nav_ [class_ "section-nav"] $ do
    sectionLink Posts     "Posts"     "/admin"
    sectionLink Galleries "Galleries" "/admin/galleries"
    sectionLink Jobs      "Jobs"      "/admin/jobs"
    where
        sectionLink :: Section -> Text -> Text -> Html ()
        sectionLink s label url =
            let cls = if s == active then "section-nav-link section-nav-link-active"
                                     else "section-nav-link"
            in with a_ [href_ url, class_ cls] (toHtml label)
