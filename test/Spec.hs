{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import Data.Pool
import Madf.Blog (routes)
import qualified Madf.Blog.DB as DB
import qualified Madf.Blog.Env as Env
import qualified Madf.Blog.Config as Config
import qualified Madf.Blog.Logger as Logger
import qualified Web.Scotty.Trans as WST
import Test.Hspec
import Test.Hspec.Wai

main :: IO ()
main = do
    let conf = Config.defaultConfig{ Config.db = Config.DBConfig ":memory:" }
    env <- Env.create conf False
    withResource (Env.pool env) DB.check
    hspec (spec env)

spec :: Env.Env -> Spec
spec env = with app $ do
    describe "GET /admin" $ do
        it "responds with 302" $ do
            get "/admin" `shouldRespondWith` 302
    describe "GET /admin/login" $ do
        it "responds with 200" $ do
            get "/admin/login" `shouldRespondWith` 200
    describe "GET /admin/api/posts" $ do
        it "responds with 401" $ do
            get "/admin/api/posts" `shouldRespondWith` 401
    where
        logger = Logger.loggerMiddleware . Env.loggerRes $ env
        app = WST.scottyAppT WST.defaultOptions (Env.runIO env) (routes logger)
