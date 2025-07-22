module Madf.Blog.Auth
    ( require
    ) where

import Data.Text
import qualified Data.Text.Lazy as DTL
import Control.Monad.Reader
import Web.Scotty.Trans
import qualified Network.HTTP.Types as NT
import qualified Madf.Blog.Env as Env
import qualified Madf.Blog.JWT as JWT

require :: ActionT Env.EnvM ()
require = do
    mt <- header "Authroization"
    case mt of
        Nothing -> status NT.unauthorized401 >> json ("No authorization token" :: Text)
        Just t -> do
            jwtEnv <- lift $ asks Env.jwt
            liftIO $ JWT.check jwtEnv (DTL.toStrict t)
