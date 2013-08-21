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
                | ViewIPAndIDP 
                | HellBanP     
                | ChangeFileRatingP
                | AdditionalMarkupP
                deriving (Show, Ord, Read, Eq, Bounded, Enum)
           
derivePersistField "Permission"
