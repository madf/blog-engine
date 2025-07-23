module Madf.Blog.Auth
    ( require
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

require :: ActionT Env.EnvM ()
require = do
    mt <- getCookie "authtoken"
    p <- decodeUtf8 . rawPathInfo <$> request
    case mt of
        Nothing -> redirectUnauthorized (Just p) "Not authorized"
        Just t -> do
            jwtEnv <- lift $ asks Env.jwt
            r <- liftIO $ JWT.check jwtEnv t
            case r of
                JWT.Error e -> redirectUnauthorized (Just p) e
                JWT.Ok -> return ()

redirectUnauthorized :: Maybe Text -> Text -> ActionT Env.EnvM ()
redirectUnauthorized from errorMessage = void $ redirect ("/admin/login?" <> q)
    where
        q = DTL.fromStrict . decodeUtf8 $ NT.renderQuery False qps
        qp = Just . encodeUtf8
        qps = case from of
            Just f -> [("from", qp f), ("error", qp errorMessage)]
            Nothing -> [("error", qp errorMessage)]
