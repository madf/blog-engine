module Madf.Blog
    ( serve
    , routes
    ) where

import Data.Pool
import Data.String (fromString)
import Data.Text (unpack)
import Web.Scotty.Trans as WS
import Network.Wai (Middleware)
import Network.Wai.Middleware.Static
import Network.Wai.Middleware.Cors
import Network.Wai.Handler.Warp (Settings, defaultSettings, setPort, setHost, setGracefulShutdownTimeout, setInstallShutdownHandler)
import System.Posix.Signals (installHandler, Handler(..), sigTERM, sigINT)
import Madf.Blog.App
import Madf.Blog.Config qualified as Config
import Madf.Blog.Env qualified as Env
import Madf.Blog.Logger qualified as Logger
import Madf.Blog.DB qualified as DB
import Madf.Blog.Admin.Routes qualified as Admin
import Madf.Blog.Public.Routes qualified as Public

serve :: Env.Env -> IO ()
serve env = do
    withResource (Env.pool env) DB.check
    let warpSettings = makeSettings (Env.config env)
        opts = WS.Options 1 warpSettings
        logger = Logger.loggerMiddleware . Env.loggerRes $ env
    scottyOptsT opts (Env.runIO env) (routes logger)

makeSettings :: Config.Config -> Settings
makeSettings conf =
    let sc = Config.server conf
        shutdownHandler closeSocket = do
            _ <- installHandler sigTERM (Catch closeSocket) Nothing
            _ <- installHandler sigINT  (Catch closeSocket) Nothing
            return ()
    in setInstallShutdownHandler shutdownHandler
     $ setGracefulShutdownTimeout (Just 30)
     $ setPort (Config.port sc)
     $ setHost (fromString $ unpack $ Config.host sc) defaultSettings

routes :: Middleware -> App ()
routes logger = do
    middleware logger
    middleware $ staticPolicy (noDots >-> addBase "static")
    middleware $ cors $ const $ Just simpleCorsResourcePolicy
        { corsRequestHeaders = "Authorization":simpleHeaders
        , corsMethods = "PUT":"DELETE":simpleMethods
        }
    options (regex ".*") $ return ()
    Admin.routes
    Public.routes
