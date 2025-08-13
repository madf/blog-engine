module Madf.Blog.Pages.Utils
    ( Option (..)
    , drawSelect
    ) where

import Data.Text
import Lucid

data Option = OptActive !Text !Text | OptDisabled !Text !Text deriving (Show)

optName :: Option -> Text
optName = \case
    OptActive n _ -> n
    OptDisabled n _ -> n

optValue :: Option -> Text
optValue = \case
    OptActive _ v -> v
    OptDisabled _ v -> v

drawSelect :: Text -> Text -> [Option] -> Maybe Text -> Html ()
drawSelect title name options value = do
    with label_ [for_ name] (toHtml title)
    select_ [name_ name, id_ name] $ do
        mapM_ (drawOption value) options

drawOption :: Maybe Text -> Option -> Html ()
drawOption v o = let attrs = value_ (optValue o) : (Prelude.map snd . Prelude.filter (`fst` o) $ flags)
                 in with option_ attrs (toHtml $ optName o)
    where
        flags =
            [ (isSelected, selected_ "")
            , (isDisabled, disabled_ "")
            ]
        isSelected o' = Just (optValue o') == v
        isDisabled = \case
            OptActive _ _ -> False
            OptDisabled _ _ -> True
