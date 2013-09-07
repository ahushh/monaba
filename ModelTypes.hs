module ModelTypes
       (
         Permission(..)
       ) where

import Prelude
import Yesod

data Permission = ManageThreadP
                | ManageBoardP
                | ManageUsersP
                | ManageConfigP
                | DeletePostsP
                | ManagePanelP
                | ManageBanP
                | EditPostsP
                | ShadowEditP
                | ViewIPAndIDP
                | ViewModlogP
                | HellBanP
                | ChangeFileRatingP
                | AdditionalMarkupP
                deriving (Show, Ord, Read, Eq, Bounded, Enum)
           
derivePersistField "Permission"
