module ModelTypes
       (
         Permission(..)
       ) where

import Prelude
import Yesod
import Control.Monad (mzero)
import Data.Text     (pack, unpack)
---------------------------------------------------------------------------------------------------------
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
           
instance ToJSON Permission where
  toJSON x = String $ pack $ show x

instance FromJSON Permission where
  parseJSON (String x) = return $ read $ unpack x
  parseJSON _          = mzero

derivePersistField "Permission"
