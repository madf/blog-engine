module Main (main) where

import System.Environment (getArgs)
import Data.Maybe
import Data.Text
import Madf.Blog
import Madf.Blog.Env

main :: IO ()
main = do
    mfn <- listToMaybe <$> getArgs
    env <- maybe defaultEnv (create . pack) mfn
    serve env
