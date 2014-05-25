 {-# LANGUAGE OverloadedStrings #-}
module ModelTypes
       (
         Permission(..)
       , GeoCountry(..)
       ) where

import Prelude
import Yesod
import Control.Monad (mzero)
import Control.Applicative ((<$>), (<*>))
--import Data.Aeson
import Data.Text     (pack, unpack, Text)
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

-- | Contains country code and name
data GeoCountry = GeoCountry { geoCountryCode' :: Text
                             , geoCountryName' :: Text
                             }
                deriving (Show, Ord, Read, Eq)

instance ToJSON GeoCountry where
  toJSON (GeoCountry code name) = object ["code" .= code, "name" .= name]

instance FromJSON GeoCountry where
  parseJSON (Object o) = GeoCountry <$>
                         o .: "code" <*>
                         o .: "name"
  parseJSON _          = mzero


derivePersistField "Permission"
derivePersistField "GeoCountry"

