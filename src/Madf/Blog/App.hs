module Madf.Blog.App
    ( App
    , Action
    , askPool
    , askConfig
    , withConn
    ) where

import Control.Monad.Reader
import Data.Pool
import Database.SQLite.Simple
import Web.Scotty.Trans
import Madf.Blog.Env qualified as Env
import Madf.Blog.Config

type App a = ScottyT Env.EnvM a
type Action a = ActionT Env.EnvM a

askPool :: Action (Pool Connection)
askPool = lift $ asks Env.pool

withConn :: (Connection -> IO a) -> Action a
withConn f = do
    pool <- askPool
    liftIO $ withResource pool f

askConfig :: Action Config
askConfig = lift $ asks Env.config
