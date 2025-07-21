module Madf.Blog.JWT
    ( Env (..)
    , Config (..)
    , create
    , defaultConfig
    , parser
    ) where

import Data.Text
import Data.Text.Read
import Data.Ini.Config
import qualified Crypto.JOSE.JWK as JOSE
import qualified Crypto.JWT as JOSE

data Env = Env
    { jwk :: !JOSE.JWK
    , alg :: !JOSE.Alg
    } deriving (Show)

data Config = Config
    { path     :: !FilePath
    , genParam :: !JOSE.KeyMaterialGenParam
    } deriving (Show)

data KeyType = HMAC | EC | RSA | EdDSA

create :: Config -> IO Env
create (Config p gp) = do
    k <- JOSE.genJWK gp
    case JOSE.bestJWSAlg k of
        Left e -> makeError e
        Right a -> return $ Env k a
    where
        makeError :: JOSE.JWTError -> IO Env
        makeError = error . show

defaultConfig :: Config
defaultConfig = Config "key.jwk" (JOSE.ECGenParam JOSE.P_384)

parser :: SectionParser Config
parser = do
    p <- fieldOf "path" string
    kt <- fieldOf "key_type" keyType
    km <- fieldOf "key_param" (keyMaterialGenParam kt)
    return $ Config p km

keyType :: Text -> Either String KeyType
keyType = \case
    "hmac" -> Right HMAC
    "ec" -> Right EC
    "rsa" -> Right RSA
    "eddsa" -> Right EdDSA
    v -> Left . unpack $ "Unknown key type: '" <> v <> "'."

keyMaterialGenParam :: KeyType -> Text -> Either String JOSE.KeyMaterialGenParam
keyMaterialGenParam HMAC t  = JOSE.OctGenParam . fst <$> decimal t
keyMaterialGenParam EC t    = JOSE.ECGenParam <$> parseCurve t
keyMaterialGenParam RSA t   = JOSE.RSAGenParam . fst <$> decimal t
keyMaterialGenParam EdDSA t = JOSE.OKPGenParam <$> parseEdCurve t

parseCurve :: Text -> Either String JOSE.Crv
parseCurve = \case
    "P_256" -> Right JOSE.P_256
    "P_384" -> Right JOSE.P_384
    "P_521" -> Right JOSE.P_521
    "Secp256k1" -> Right JOSE.Secp256k1
    v -> Left . unpack $ "Unknown elliptic curve: '" <> v <> "'."

parseEdCurve :: Text -> Either String JOSE.OKPCrv
parseEdCurve = \case
    "Ed25519" -> Right JOSE.Ed25519
    "Ed448" -> Right JOSE.Ed448
    "X25519" -> Right JOSE.X25519
    "X448" -> Right JOSE.X448
    v -> Left . unpack $ "Unknown Edwards curve: '" <> v <> "'."
