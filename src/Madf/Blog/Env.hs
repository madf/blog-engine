{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Madf.Blog.Env
    ( Env (..)
    , EnvM (..)
    , runIO
    , create
    , destroy
    ) where

import Data.Pool hiding (createPool)
import Data.Text
import Database.SQLite.Simple
import Control.Monad.Reader
import Control.Monad.IO.Unlift (MonadUnliftIO(..))
import qualified Madf.Blog.Config as C
import qualified Madf.Blog.Job as Job
import qualified Madf.Blog.JWT as JWT
import qualified Madf.Blog.Logger as Logger

data Env = Env
    { config    :: !C.Config
    , pool      :: !(Pool Connection)
    , jwt       :: !JWT.Env
    , loggerRes :: !Logger.LoggerResource
    , job       :: !Job.Env
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

create :: C.Config -> Bool -> IO Env
create conf regenKey = do
    p <- createPool conf
    je <- JWT.createEnv (C.jwt conf) regenKey
    lr <- Logger.create conf
    jobEnv <- Job.initEnv (C.job conf)
    return $ Env conf p je lr jobEnv

destroy :: Env -> IO ()
destroy env = do
    Job.destroyEnv (job env)
    Logger.cleanup (loggerRes env)
    destroyAllResources (pool env)
