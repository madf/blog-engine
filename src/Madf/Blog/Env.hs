{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Madf.Blog.Env
    ( Env (..)
    , EnvM (..)
    , runIO
    , create
    , defaultEnv
    ) where

import Data.Pool
import Data.Text
import Database.SQLite.Simple
import Control.Monad.Reader
import Control.Monad.IO.Unlift (MonadUnliftIO(..))
import qualified Madf.Blog.Config as C

data Env = Env
    { config :: !C.Config
    , pool   :: !(Pool Connection)
    }

newtype EnvM a = EnvM
    { runEnvM :: ReaderT Env IO a
    } deriving (Applicative, Functor, Monad, MonadIO, MonadReader Env, MonadUnliftIO)

runIO :: Env -> EnvM a -> IO a
runIO env m = runReaderT (runEnvM m) env

create :: Text -> IO Env
create fp = do
    ec <- C.readFile fp
    case ec of
        Left e -> error (unpack e)
        Right c -> do
            p <- newPool $ defaultPoolConfig (open (unpack . C.path $ C.db c)) close 3600 10
            return $ Env c p

defaultEnv :: IO Env
defaultEnv = do
    p <- newPool $ defaultPoolConfig (open (unpack . C.path $ C.db C.defaultConfig)) close 3600 10
    return $ Env C.defaultConfig p
