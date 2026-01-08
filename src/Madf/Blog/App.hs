module Madf.Blog.App
    ( App
    , Action
    , withConn
    ) where

import Control.Monad.Reader
import Data.Pool
import Database.SQLite.Simple
import Web.Scotty.Trans
import Madf.Blog.Env qualified as Env

type App a = ScottyT Env.EnvM a
type Action a = ActionT Env.EnvM a

withConn :: (Connection -> IO a) -> Action a
withConn f = do
    pool <- asks Env.pool
    liftIO $ withResource pool f
