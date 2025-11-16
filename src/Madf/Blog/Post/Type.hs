module Madf.Blog.Post.Type
    ( Type (..)
    , typeName
    , splitType
    , makeType
    , shouldRender
    , shouldList
    ) where

import Data.Text (Text)
import Data.Aeson

data Type = Public | Unlisted !Text | Private !Text | Unknown !Text deriving (Show, Eq)

typeName :: Type -> Text
typeName = \case
    Public     -> "public"
    Unlisted _ -> "unlisted"
    Private _  -> "private"
    Unknown _  -> "unknown"

splitType :: Type -> (Text, Text)
splitType v = case v of
    Public     -> (typeName v, mempty)
    Unlisted r -> (typeName v, r)
    Private r  -> (typeName v, r)
    Unknown r  -> (typeName v, r)

makeType :: Text -> Text -> Type
makeType t r = case t of
    "public"   -> Public
    "unlisted" -> Unlisted r
    "private"  -> Private r
    _          -> Unknown r

shouldRender :: Type -> Bool
shouldRender = \case
    Private _ -> False
    _         -> True

shouldList :: Type -> Bool
shouldList = \case
    Public -> True
    _      -> False

instance ToJSON Type
    where
        toJSON v = let (t, r) = splitType v
                   in object [ "type" .= t, "reason" .= r ]
        toEncoding v = let (t, r) = splitType v
                       in pairs ( "type" .= t <> "reason" .= r )

instance FromJSON Type
    where
        parseJSON = withObject "Madf.Blog.Post.Type.Storage" $ \o -> do
            t <- o .: "type"
            r <- o .: "reason"
            return $ makeType t r
