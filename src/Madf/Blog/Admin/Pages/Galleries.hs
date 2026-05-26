module Madf.Blog.Admin.Pages.Galleries
    ( galleries
    ) where

import Lucid
import Madf.Blog.Galleries (Gallery)

galleries :: [Gallery] -> Html ()
galleries _ = p_ "Coming soon."
