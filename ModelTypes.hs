module ModelTypes
       (
         RoleOfPerson(..)
       ) where

import Prelude
import Yesod

data RoleOfPerson = Moderator | Admin
  deriving (Show, Read, Eq, Ord, Bounded, Enum)
           
derivePersistField "RoleOfPerson"

