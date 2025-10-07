module Madf.Blog
    ( serve
    , routes
    ) where

import Data.Pool
import Data.ByteString (useAsCStringLen)
import Data.String (fromString)
import Data.Text (unpack)
import Control.Exception (bracket)
import Web.Scotty.Trans as WS
import Network.Wai (Middleware)
import Network.Wai.Middleware.Static
import Network.Wai.Middleware.Cors
import Network.Wai.Middleware.RequestLogger
import Network.Wai.Handler.Warp (Settings, defaultSettings, setPort, setHost)
import System.Log.FastLogger
import System.IO (stdout)
import System.Posix.Syslog (syslog, Priority (..))
import Madf.Blog.App
import Madf.Blog.Config qualified as Config
import Madf.Blog.Env qualified as Env
import Madf.Blog.DB qualified as DB
import Madf.Blog.Admin.Routes qualified as Admin
import Madf.Blog.Public.Routes qualified as Public

serve :: Env.Env -> IO ()
serve env = do
    withResource (Env.pool env) DB.check
    bracket (makeLogger (Env.config env)) cleanupLogger $ \loggerRes -> do
        let warpSettings = makeSettings (Env.config env)
            opts = WS.Options 1 warpSettings
        scottyOptsT opts (Env.runIO env) (routes $ loggerMiddleware loggerRes)

makeSettings :: Config.Config -> Settings
makeSettings conf =
    let sc = Config.server conf
    in setPort (Config.port sc)
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

data LoggerResource = LoggerResource
    { loggerMiddleware :: !Middleware
    , cleanup          :: !(IO ())
    }

makeLogger :: Config.Config -> IO LoggerResource
makeLogger conf = do
    (dest, cleanupAction) <- mkDestination (Config.destination . Config.logging $ conf)
    mw <- mkRequestLogger defaultRequestLoggerSettings
        { outputFormat = Detailed (Config.debug . Config.server $ conf)
        , destination = dest
        }
    return $ LoggerResource mw cleanupAction

cleanupLogger :: LoggerResource -> IO ()
cleanupLogger = cleanup

mkDestination :: Config.LogDestination -> IO (Destination, IO ())
mkDestination d = case d of
    Config.Stdout  -> return (Handle stdout, return ())
    Config.File fp -> do
        loggerSet <- newFileLoggerSet defaultBufSize fp
        return (Logger loggerSet, rmLoggerSet loggerSet)
    Config.Syslog  -> return (Callback syslogCallback, return ())

syslogCallback :: LogStr -> IO ()
syslogCallback ls = do
    let bs = fromLogStr ls
    useAsCStringLen bs (syslog Nothing Info)
