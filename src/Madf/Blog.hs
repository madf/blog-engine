module Madf.Blog
    ( serve
    , routes
    ) where

import Data.Pool
import Web.Scotty.Trans as WS
import Network.Wai (Middleware)
import Network.Wai.Middleware.Static
import Network.Wai.Middleware.Cors
import Network.Wai.Middleware.RequestLogger
import Madf.Blog.App
import Madf.Blog.Config (Config(..), LoggingConfig(..), LogLevel(..))
import Madf.Blog.Env qualified as Env
import Madf.Blog.DB qualified as DB
import Madf.Blog.Admin.Routes qualified as Admin
import Madf.Blog.Public.Routes qualified as Public

serve :: Env.Env -> IO ()
serve env = do
    withResource (Env.pool env) DB.check
    scottyOptsT WS.defaultOptions (Env.runIO env) (routes env)

routes :: Env.Env -> App ()
routes env = do
    let loggerMiddleware = makeLogger (Env.config env)
    middleware loggerMiddleware
    middleware $ staticPolicy (noDots >-> addBase "static")
    middleware $ cors $ const $ Just simpleCorsResourcePolicy
        { corsRequestHeaders = "Authorization":simpleHeaders
        , corsMethods = "PUT":"DELETE":simpleMethods
        }
    options (regex ".*") $ return ()
    Admin.routes
    Public.routes

makeLogger :: Config -> Middleware
makeLogger conf = case level . logging $ conf of
    Debug   -> logStdoutDev
    Info    -> logStdout
    Warning -> logStdout
    Error   -> logStdout
