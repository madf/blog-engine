module Madf.Blog.JWT
    ( Env (..)
    , Config (..)
    , Result (..)
    , create
    , defaultConfig
    , parser
    , issue
    , check
    , renew
    ) where

import Data.Text
import Data.Text.Read
import Data.Text.Encoding
import qualified Data.ByteString.Lazy as LBS
import Data.Ini.Config
import Data.Time
import Control.Lens
import qualified Crypto.JOSE.JWK as JOSE
import qualified Crypto.JWT as JOSE

data Env = Env
    { jwk        :: !JOSE.JWK
    , alg        :: !JOSE.Alg
    , validation :: !JOSE.JWTValidationSettings
    , config     :: !Config
    }

data Config = Config
    { path          :: !FilePath
    , genParam      :: !JOSE.KeyMaterialGenParam
    , keyExp        :: !NominalDiffTime
    , keyIssuer     :: !Text
    } deriving (Show)

data KeyType = HMAC | EC | RSA | EdDSA

create :: Config -> IO Env
create conf = do
    k <- JOSE.genJWK (genParam conf)
    case JOSE.bestJWSAlg k of
        Left e -> makeError e
        Right a -> return $ Env k a (JOSE.defaultJWTValidationSettings (== "admin")) conf
    where
        makeError :: JOSE.JWTError -> IO Env
        makeError = error . show

defaultConfig :: Config
defaultConfig = Config "key.jwk" (JOSE.ECGenParam JOSE.P_384) 3600 "Madf.Blog"

parser :: SectionParser Config
parser = do
    p <- fieldOf "path" string
    kt <- fieldOf "key_type" keyType
    km <- fieldOf "key_param" (keyMaterialGenParam kt)
    ke <- fieldOf "key_expiration" keyExpiration
    ki <- fieldOf "key_issuer" string
    return $ Config p km ke ki

keyType :: Text -> Either String KeyType
keyType = \case
    "hmac" -> Right HMAC
    "ec" -> Right EC
    "rsa" -> Right RSA
    "eddsa" -> Right EdDSA
    v -> Left . unpack $ "Unknown key type: '" <> v <> "'."

keyExpiration :: Text -> Either String NominalDiffTime
keyExpiration t = fromInteger <$> number t

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

make :: Env -> IO LBS.ByteString
make env = do
    t <- getCurrentTime
    r <- JOSE.runJOSE $ do
        JOSE.encodeCompact <$> JOSE.signClaims (jwk env) (JOSE.newJWSHeader ((), alg env)) (claims t)
    case r of
        Left e -> makeError e
        Right v -> return v
    where
        claims t = JOSE.emptyClaimsSet
            & JOSE.claimIss ?~ (JOSE.string # (keyIssuer . config $ env))
            & JOSE.claimAud ?~ JOSE.Audience ["admin"]
            & JOSE.claimIat ?~ JOSE.NumericDate t
            & JOSE.claimExp ?~ JOSE.NumericDate (addUTCTime (keyExp . config $ env) t)
        makeError :: JOSE.JWTError -> IO LBS.ByteString
        makeError = error . show

issue :: Env -> IO Text
issue env = decodeUtf8 . LBS.toStrict <$> make env

data Result = Error Text | Ok deriving (Show)

verify :: Env -> LBS.ByteString -> IO Result
verify env t = do
    case JOSE.decodeCompact t of
        Left e -> makeError e
        Right jwt -> do
            r <- JOSE.runJOSE $ JOSE.verifyClaims (validation env) (jwk env) jwt
            case r of
                Left e -> makeError e
                Right _ -> return Ok
    where
        makeError :: JOSE.JWTError -> IO Result
        makeError = return . Error . pack . show

check :: Env -> Text -> IO Result
check env t = verify env (LBS.fromStrict . encodeUtf8 $ t)

renew :: Env -> Text -> IO Text
renew env t = check env t >> issue env
