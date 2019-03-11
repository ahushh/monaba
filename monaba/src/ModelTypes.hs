{-# LANGUAGE FlexibleInstances #-} 
module ModelTypes
       (
         Permission(..)
       , FileType(..)
       , GeoCountry(..)
       , WordfilterAction(..)
       , WordfilterDataType(..)
       , IP.IP(..)
       ) where

import Prelude
import Data.Text (unpack, Text)
import Control.Monad (mzero)
import Control.Applicative ((<$>), (<*>))
import Yesod
import  qualified Data.IP as IP

-- | Contains country code and name
data GeoCountry = GeoCountry { geoCountryCode' :: Text
                             , geoCountryName' :: Text
                             }
                deriving (Show, Ord, Read, Eq)

instance ToJSON GeoCountry where
  toJSON (GeoCountry code name) = object ["code" .= code, "name" .= name]

instance FromJSON GeoCountry where
  parseJSON (Object o) = GeoCountry  <$>
                         o .: "code" <*>
                         o .: "name"
  parseJSON _          = mzero

data Permission = ManageThreadP
                | ManageBoardP
                | ManageUsersP
                | ManageConfigP
                | DeletePostsP
                | ManagePanelP
                | ManageBanP
                | EditPostsP
                | ViewModlogP
                | AdditionalMarkupP
                | ViewIPAndIDP
                | HellBanP
                | ChangeFileRatingP
                | ShadowEditP
                | AppControlP
                | WordfilterP
                | ReportsP
                deriving (Show, Ord, Read, Eq, Bounded, Enum)
           
data WordfilterDataType = WordfilterWords | WordfilterExactMatch | WordfilterRegex
                   deriving (Show, Ord, Read, Eq, Bounded, Enum)

data WordfilterAction = WordfilterBan | WordfilterHB | WordfilterHBHide | WordfilterDeny | WordfilterReplace
                     deriving (Show, Ord, Read, Eq, Bounded, Enum)

data Ternary = Enabled | Disabled | Required
             deriving (Show, Ord, Read, Eq, Bounded, Enum)

data FileType = FileVideo | FileImage | FileAudio | FileFlash | FileDoc | FileSource | FileArchive | FileUndetected
              deriving (Show, Ord, Read, Eq, Bounded, Enum)

instance ToJSON FileType where
  toJSON x = toJSON $ show x

instance FromJSON FileType where
  parseJSON (String x) = return $ read $ unpack x
  parseJSON _          = return FileUndetected

derivePersistField "Permission"
derivePersistField "FileType"
derivePersistField "GeoCountry"
derivePersistField "WordfilterDataType"
derivePersistField "WordfilterAction"
derivePersistField "IP.IP"
