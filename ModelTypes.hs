module ModelTypes
       (
         Permission(..)
       ) where

import Prelude
import Yesod
import Data.Time (UTCTime)
import Data.Text (Text)

data Permission = ManageThreadP |
                  ManageBoardP  |
                  ManageUsersP  |
                  ManageConfigP |
                  DeletePostsP  |
                  ManagePanelP  |
                  ManageBanP    |
                  EditPostsP
                deriving (Show, Ord, Read, Eq, Bounded, Enum)
           
derivePersistField "Permission"
