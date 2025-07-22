module Madf.Blog.Login
    ( verify
    ) where

import Data.Text
import Data.Password.Argon2
import Madf.Blog.Config

verify :: AdminConfig -> Text -> Text -> Bool
verify conf l p
    | login conf == l && checkPassword (mkPassword p) (passwordHash conf) == PasswordCheckSuccess = True
    | otherwise = False
