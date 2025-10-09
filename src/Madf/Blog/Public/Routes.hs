module Madf.Blog.Public.Routes
    ( routes
    ) where

import Data.Text qualified as DT
import Data.Text.Lazy qualified as DTL
import Web.Scotty.Trans
import Network.HTTP.Types qualified as NT
import Madf.Blog.App
import Madf.Blog.Config qualified as Config
import Madf.Blog.Slug qualified as Slug
import Madf.Blog.Image qualified as Image

routes :: App ()
routes = do
    get "/blog" getIndexPage
    get "/blog/:year" getYearIndexPage
    get "/blog/:year/:fileName" getPostPage
    get "/blog/:year/:postSlug/:fileName" getPostImage

getIndexPage :: Action ()
getIndexPage = do
    setHeader "Content-Type" "text/html"
    dd <- Config.destDir . Config.main <$> askConfig
    file (DT.unpack $ dd <> "/index.html")

getYearIndexPage :: Action ()
getYearIndexPage = do
    year <- pathParam "year"
    setHeader "Content-Type" "text/html"
    dd <- Config.destDir . Config.main <$> askConfig
    file (DT.unpack $ dd <> "/" <> year <> "/index.html")

getPostPage :: Action ()
getPostPage = do
    year <- pathParam "year"
    fn <- pathParam "fileName"
    setHeader "Content-Type" "text/html"
    dd <- Config.destDir . Config.main <$> askConfig
    file (DT.unpack $ dd <> "/" <> year <> "/" <> fn)

getPostImage :: Action ()
getPostImage = do
    year <- pathParam "year"
    slug <- pathParam "postSlug"
    fn <- pathParam "fileName"
    mi <- withConn $ \conn -> Image.getByFileName conn slug fn
    case mi of
        Nothing -> status NT.notFound404
        Just img -> do
            setHeader "Content-Type" (DTL.fromStrict $ Image.imageMIMEType img)
            dd <- Config.destDir . Config.main <$> askConfig
            file (DT.unpack $ dd <> "/" <> year <> "/" <> Slug.unSlug slug <> "/" <> fn)
