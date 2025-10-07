{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Madf.Blog.Env
    ( Env (..)
    , EnvM (..)
    , LoggerResource (..)
    , runIO
    , create
    , destroy
    ) where

import Data.Pool hiding (createPool)
import Data.Text
import Data.ByteString (useAsCStringLen)
import Database.SQLite.Simple
import Control.Monad.Reader
import Control.Monad.IO.Unlift (MonadUnliftIO(..))
import Network.Wai (Middleware)
import Network.Wai.Middleware.RequestLogger
import System.Log.FastLogger
import System.IO (stdout)
import System.Posix.Syslog (syslog, Priority (..))
import qualified Madf.Blog.Config as C
import qualified Madf.Blog.JWT as JWT

data LoggerResource = LoggerResource
    { loggerMiddleware :: !Middleware
    , loggerCleanup    :: !(IO ())
    }

data Env = Env
    { config    :: !C.Config
    , pool      :: !(Pool Connection)
    , jwt       :: !JWT.Env
    , loggerRes :: !LoggerResource
    }

newtype EnvM a = EnvM
    { runEnvM :: ReaderT Env IO a
    } deriving (Applicative, Functor, Monad, MonadIO, MonadReader Env, MonadUnliftIO)

runIO :: Env -> EnvM a -> IO a
runIO env m = runReaderT (runEnvM m) env

openWithFK :: C.Config -> IO Connection
openWithFK conf = do
    conn <- open (unpack . C.path . C.db $ conf)
    execute_ conn "PRAGMA foreign_keys = ON"
    return conn

createPool :: C.Config -> IO (Pool Connection)
createPool conf = do
    let pcfg = defaultPoolConfig (openWithFK conf) close 3600 10
    newPool $ setNumStripes (Just 2) pcfg

makeLogger :: C.Config -> IO LoggerResource
makeLogger conf = do
    (dest, cleanupAction) <- mkDestination (C.destination . C.logging $ conf)
    mw <- mkRequestLogger defaultRequestLoggerSettings
        { outputFormat = Detailed (C.debug . C.server $ conf)
        , destination = dest
        }
    return $ LoggerResource mw cleanupAction

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

create :: C.Config -> Bool -> IO Env
create conf regenKey = do
    p <- createPool conf
    je <- JWT.createEnv (C.jwt conf) regenKey
    lr <- makeLogger conf
    return $ Env conf p je lr

destroy :: Env -> IO ()
destroy env = do
    loggerCleanup (loggerRes env)
    destroyAllResources (pool env)
