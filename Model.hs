module Model where

import Prelude
import Yesod
import Data.Text     (Text)
import Database.Persist.Quasi
import Data.Typeable (Typeable)
import Data.Time     (UTCTime) -- appears in model

import Yesod.Auth.HashDB (HashDBUser(..))
import ModelTypes
-- You can define all of your database entities in the entities file.
-- You can find more information on persistent and how to declare entities
-- at:
-- http://www.yesodweb.com/book/persistent/

share [mkPersist sqlOnlySettings, mkMigrate "migrateAll"]
    $(persistFileWith lowerCaseSettings "config/models")

instance HashDBUser Person where
    userPasswordHash = Just . personPassword
    userPasswordSalt = Just . personSalt
    setSaltAndPasswordHash s h p = p { personSalt     = s
                                     , personPassword = h
                                     }
