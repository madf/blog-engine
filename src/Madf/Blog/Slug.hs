{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Madf.Blog.Slug
    ( Type (..)
    , unSlug
    , get
    , withId
    ) where

import Data.Text
import Data.Text.Encoding
import qualified Data.ByteString.Builder as BSB
import qualified Data.ByteString.Lazy.Char8 as BSL
import Crypto.Random
import Database.SQLite.Simple.FromField
import Database.SQLite.Simple.ToField
import Data.Aeson (ToJSON, FromJSON)
import Web.Scotty (Parsable)
import Madf.Blog.Ids

newtype Type = Type Text deriving (Show, ToField, FromField, ToJSON, FromJSON, Parsable)

unSlug :: Type -> Text
unSlug (Type v) = v

get :: Int -> Text -> IO Type
get len suffix = do
    (bs, _) <- randomBytesGenerate halfLen <$> drgNew
    return $ make bs
    where
        halfLen = len `div` 2 + 1
        make = Type . (<> suffix) . Data.Text.take len . decodeUtf8 . BSL.toStrict . BSB.toLazyByteString . BSB.byteStringHex

withId :: Int -> Id a -> IO Type
withId len = get len . ("-" <>) . toText
