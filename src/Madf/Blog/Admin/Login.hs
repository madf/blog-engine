module Madf.Blog.Admin.Login
    ( verify
    ) where

import Data.Text
import Data.Password.Argon2
import Madf.Blog.Config

verify :: AdminConfig -> Text -> Text -> Bool
-- seq forces checkPassword to run even when loginOk is False, so a wrong
-- login can't be timed against a wrong password via short-circuiting.
verify conf l p = passOk `seq` (loginOk && passOk)
    where
        loginOk = login conf == l
        passOk = checkPassword (mkPassword p) (passwordHash conf) == PasswordCheckSuccess
