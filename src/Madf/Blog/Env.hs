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

data Env = Env
    { config :: !C.Config
    , pool   :: !(Pool Connection)
    }

newtype EnvM a = EnvM
    { runEnvM :: ReaderT Env IO a
    } deriving (Applicative, Functor, Monad, MonadIO, MonadReader Env, MonadUnliftIO)

runIO :: Env -> EnvM a -> IO a
runIO env m = runReaderT (runEnvM m) env

createPool :: C.Config -> IO (Pool Connection)
createPool conf = do
    let pcfg = defaultPoolConfig (open (unpack . C.path $ C.db conf)) close 3600 10
    newPool $ setNumStripes (Just 2) pcfg

create :: Text -> IO Env
create fp = do
    ec <- C.readFile fp
    case ec of
        Left e -> error (unpack e)
        Right c -> do
            p <- createPool c
            return $ Env c p

defaultEnv :: IO Env
defaultEnv = do
    p <- createPool C.defaultConfig
    return $ Env C.defaultConfig p
