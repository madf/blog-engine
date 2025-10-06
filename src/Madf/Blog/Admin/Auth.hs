module Madf.Blog.Admin.Auth
    ( require
    , requireNoRedirect
    , redirectUnauthorized
    ) where

import Data.Text
import Data.Text.Encoding
import qualified Data.Text.Lazy as DTL
import Control.Monad
import Control.Monad.Reader
import Web.Scotty.Trans
import Web.Scotty.Cookie
import Network.Wai
import qualified Network.HTTP.Types as NT
import qualified Madf.Blog.Env as Env
import qualified Madf.Blog.JWT as JWT

data Result = Ok | Error Text deriving (Show)

check :: ActionT Env.EnvM Result
check = do
    mt <- getCookie "authtoken"
    case mt of
        Nothing -> return $ Error "Not authorized"
        Just t -> do
            jwtEnv <- lift $ asks Env.jwt
            r <- liftIO $ JWT.check jwtEnv t
            case r of
                JWT.Error e -> return $ Error e
                JWT.Ok -> return Ok

require :: ActionT Env.EnvM ()
require = do
    r <- check
    p <- decodeUtf8 . rawPathInfo <$> request
    case r of
        Ok -> return ()
        Error e -> redirectUnauthorized (Just p) e

requireNoRedirect :: ActionT Env.EnvM ()
requireNoRedirect = do
    r <- check
    case r of
        Ok -> return ()
        Error e -> status NT.unauthorized401 >> json e

redirectUnauthorized :: Maybe Text -> Text -> ActionT Env.EnvM ()
redirectUnauthorized from errorMessage = void $ redirect ("/admin/login?" <> q)
    where
        q = DTL.fromStrict . decodeUtf8 $ NT.renderQuery False qps
        qp = Just . encodeUtf8
        qps = case from of
            Just f -> [("from", qp f), ("error", qp errorMessage)]
            Nothing -> [("error", qp errorMessage)]
