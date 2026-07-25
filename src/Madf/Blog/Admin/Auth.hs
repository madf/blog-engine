module Madf.Blog.Admin.Auth
    ( requireCookie
    , requireHeader
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

-- Check Authorization header (for API)
checkHeader :: ActionT Env.EnvM Result
checkHeader = do
    mAuthHeader <- header "Authorization"
    case mAuthHeader of
        Nothing -> return $ Error "Missing Authorization header"
        Just authHeader ->
            case stripPrefix "Bearer " (DTL.toStrict authHeader) of
                Just token -> checkToken token
                Nothing -> return $ Error "Invalid Authorization header format"

-- Check cookie (for pages)
checkCookie :: ActionT Env.EnvM Result
checkCookie = do
    mt <- getCookie "authtoken"
    case mt of
        Nothing -> return $ Error "Not authorized"
        Just t -> checkToken t

-- Common token validation logic
checkToken :: Text -> ActionT Env.EnvM Result
checkToken token = do
    jwtEnv <- lift $ asks Env.jwt
    r <- liftIO $ JWT.check jwtEnv token
    case r of
        JWT.Error e -> return $ Error e
        JWT.Ok -> return Ok

-- For HTML pages - redirects to login on failure
requireCookie :: ActionT Env.EnvM ()
requireCookie = do
    r <- checkCookie
    p <- decodeUtf8 . rawPathInfo <$> request
    case r of
        Ok -> return ()
        Error e -> redirectUnauthorized (Just p) e

-- For API endpoints - returns 401 JSON on failure
requireHeader :: ActionT Env.EnvM ()
requireHeader = do
    r <- checkHeader
    case r of
        Ok -> return ()
        Error e -> status NT.unauthorized401 >> json e >> finish

redirectUnauthorized :: Maybe Text -> Text -> ActionT Env.EnvM ()
redirectUnauthorized from errorMessage = void $ redirect ("/admin/login?" <> q)
    where
        q = DTL.fromStrict . decodeUtf8 $ NT.renderQuery False qps
        qp = Just . encodeUtf8
        qps = case from of
            Just f -> [("from", qp f), ("error", qp errorMessage)]
            Nothing -> [("error", qp errorMessage)]
