module Madf.Blog
    ( serve
    , routes
    ) where

import Data.Pool
import Data.ByteString.Unsafe (unsafeUseAsCStringLen)
import Web.Scotty.Trans as WS
import Network.Wai (Middleware)
import Network.Wai.Middleware.Static
import Network.Wai.Middleware.Cors
import Network.Wai.Middleware.RequestLogger
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
    logger <- makeLogger (Env.config env)
    scottyOptsT WS.defaultOptions (Env.runIO env) (routes logger)

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

makeLogger :: Config.Config -> IO Middleware
makeLogger conf = do
    dest <- mkDestination (Config.destination lc)
    mkRequestLogger defaultRequestLoggerSettings
        { outputFormat = Detailed (Config.debug lc)
        , destination = dest
        }
  where
    lc = Config.logging conf

mkDestination :: Config.LogDestination -> IO Destination
mkDestination d = case d of
    Config.Stdout  -> return $ Handle stdout
    Config.File fp -> Logger <$> newFileLoggerSet defaultBufSize fp
    Config.Syslog  -> return $ Callback syslogCallback

syslogCallback :: LogStr -> IO ()
syslogCallback ls = unsafeUseAsCStringLen (fromLogStr ls) (syslog Nothing Info)
