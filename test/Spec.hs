{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import Madf.Blog (routes)
import qualified Madf.Blog.Env as Env
import qualified Madf.Blog.Config as Config
import qualified Web.Scotty.Trans as WST
import Test.Hspec
import Test.Hspec.Wai

main :: IO ()
main = do
    env <- Env.defaultEnv
    hspec (spec env)

spec :: Env.Env -> Spec
spec env = with app $ do
    describe "GET /api/health" $ do
        it "responds with 200" $ do
            get "/api/health" `shouldRespondWith` 200
        it "responds with true" $ do
            get "/api/health" `shouldRespondWith` "true"
    where
        base = Config.urlBase . Config.main . Env.config $ env
        app = WST.scottyAppT WST.defaultOptions (Env.runIO env) (routes base)
