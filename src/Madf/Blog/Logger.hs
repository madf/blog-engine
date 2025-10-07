module Madf.Blog.Logger
    ( LoggerResource (..)
    , create
    , cleanup
    ) where

import Data.ByteString (useAsCStringLen)
import Network.Wai (Middleware)
import Network.Wai.Middleware.RequestLogger
import System.Log.FastLogger
import System.IO (stdout)
import System.Posix.Syslog (syslog, Priority (..))
import qualified Madf.Blog.Config as C

data LoggerResource = LoggerResource
    { loggerMiddleware :: !Middleware
    , loggerCleanup    :: !(IO ())
    }

create :: C.Config -> IO LoggerResource
create conf = do
    (dest, cleanupAction) <- mkDestination (C.destination . C.logging $ conf)
    mw <- mkRequestLogger defaultRequestLoggerSettings
        { outputFormat = Detailed (C.debug . C.server $ conf)
        , destination = dest
        }
    return $ LoggerResource mw cleanupAction

cleanup :: LoggerResource -> IO ()
cleanup = loggerCleanup

mkDestination :: C.LogDestination -> IO (Destination, IO ())
mkDestination d = case d of
    C.Stdout  -> return (Handle stdout, return ())
    C.File fp -> do
        loggerSet <- newFileLoggerSet defaultBufSize fp
        return (Logger loggerSet, rmLoggerSet loggerSet)
    C.Syslog  -> return (Callback syslogCallback, return ())

syslogCallback :: LogStr -> IO ()
syslogCallback ls = do
    let bs = fromLogStr ls
    useAsCStringLen bs (syslog Nothing Info)
