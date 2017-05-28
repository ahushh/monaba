{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveGeneric #-}
module Request ( PostRequest(..)
               , EditPostRequest(..)
               ) where

import Prelude
import Data.Text
import Data.Aeson
import GHC.Generics

data PostRequest = PostRequest { name :: Text
                               , title :: Text
                               , message :: Text
                               , captcha :: Maybe Text
                               , password :: Text
                               , destPost :: Maybe Int
                               , files :: [Int]
                               , parent :: Int
                               , board :: Text
                               , nobump :: Bool
                               } deriving Generic

data EditPostRequest = EditPostRequest { editMessage :: Text
                                       , editPassword :: Text
                                       }  deriving Generic

instance FromJSON PostRequest
instance FromJSON EditPostRequest
