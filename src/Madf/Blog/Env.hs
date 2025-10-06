{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Madf.Blog.Env
    ( Env (..)
    , EnvM (..)
    , runIO
    , create
    , defaultEnv
    ) where

import Data.Pool hiding (createPool)
import Data.Text
import Database.SQLite.Simple
import Control.Monad.Reader
import Control.Monad.IO.Unlift (MonadUnliftIO(..))
import qualified Madf.Blog.Config as C
import qualified Madf.Blog.JWT as JWT
import Madf.Blog.Error

data Env = Env
    { config :: !C.Config
    , pool   :: !(Pool Connection)
    , jwt    :: !JWT.Env
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

create :: Text -> Bool -> IO Env
create fp regenKey = do
    ec <- C.readFile fp
    case ec of
        Left e -> throwBlogError (ConfigError e)
        Right c -> do
            p <- createPool c
            je <- JWT.createEnv (C.jwt c) regenKey
            return $ Env c p je

defaultEnv :: IO Env
defaultEnv = do
    p <- createPool C.defaultConfig
    je <- JWT.createEnv JWT.defaultConfig False
    return $ Env C.defaultConfig p je
