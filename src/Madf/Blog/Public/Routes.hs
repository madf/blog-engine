module Madf.Blog.Public.Routes
    ( routes
    ) where

import Data.Text qualified as DT
import Data.Text.Lazy qualified as DTL
import Control.Monad.Reader
import Web.Scotty.Trans
import Network.HTTP.Types qualified as NT
import Madf.Blog.App
import Madf.Blog.Env qualified as Env
import Madf.Blog.Config qualified as Config
import Madf.Blog.Slug qualified as Slug
import Madf.Blog.Image qualified as Image
import Madf.Blog.Time qualified as Time
import Madf.Blog.ToText (toText)

routes :: App ()
routes = do
    get "/blog" getIndexPage
    get "/blog/:year" getYearIndexPage
    get "/blog/:year/:fileName" getPostPage
    get "/blog/:year/:postSlug/:fileName" getPostImage

destDir :: Action DT.Text
destDir = asks $ Config.destDir . Config.main . Env.config

sanitizePath :: DT.Text -> Action DT.Text
sanitizePath t
    | DT.null t || DT.any (== '/') t || t == ".." = status NT.badRequest400 >> finish
    | otherwise                                   = return t

getIndexPage :: Action ()
getIndexPage = do
    setHeader "Content-Type" "text/html"
    dd <- destDir
    file (DT.unpack $ dd <> "/index.html")

getYearIndexPage :: Action ()
getYearIndexPage = do
    year <- pathParam "year" :: Action Time.Year
    setHeader "Content-Type" "text/html"
    dd <- destDir
    file (DT.unpack $ dd <> "/" <> toText year <> "/index.html")

getPostPage :: Action ()
getPostPage = do
    year <- pathParam "year" :: Action Time.Year
    fn <- pathParam "fileName" >>= sanitizePath
    setHeader "Content-Type" "text/html"
    dd <- destDir
    file (DT.unpack $ dd <> "/" <> toText year <> "/" <> fn)

getPostImage :: Action ()
getPostImage = do
    year <- pathParam "year" :: Action Time.Year
    slug <- pathParam "postSlug"
    _ <- sanitizePath (Slug.unSlug slug)
    fn <- pathParam "fileName" >>= sanitizePath
    mi <- withConn $ \conn -> Image.getByFileName conn slug fn
    case mi of
        Nothing -> status NT.notFound404
        Just img -> do
            setHeader "Content-Type" (DTL.fromStrict $ Image.imageMIMEType img)
            dd <- destDir
            file (DT.unpack $ dd <> "/" <> toText year <> "/" <> Slug.unSlug slug <> "/" <> fn)
