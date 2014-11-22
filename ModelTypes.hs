module ModelTypes
       (
         Permission(..)
       , FileType(..)
       ) where

import Prelude
import Data.Text (unpack)
import Yesod

data Permission = ManageThreadP |
                  ManageBoardP  |
                  ManageUsersP  |
                  ManageConfigP |
                  DeletePostsP  |
                  ManagePanelP  |
                  ManageBanP    |
                  EditPostsP    |
                  AdditionalMarkupP
                deriving (Show, Ord, Read, Eq, Bounded, Enum)
           
data Ternary = Enabled | Disabled | Required
             deriving (Show, Ord, Read, Eq, Bounded, Enum)

data FileType = FileVideo | FileImage | FileAudio | FileFlash | FileDoc | FileSource | FileArchive | FileUndetected
              deriving (Show, Ord, Read, Eq, Bounded, Enum)

instance ToJSON FileType where
  toJSON x = toJSON $ show x

instance FromJSON FileType where
  parseJSON (String x) = return $ read $ unpack x

derivePersistField "Permission"
derivePersistField "FileType"
