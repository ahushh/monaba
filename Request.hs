{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveGeneric #-}
module Request ( PostRequest(..)
               ) where

import Prelude
import Data.Text
import Data.Aeson
import GHC.Generics

data PostRequest = PostRequest { name :: Maybe Text
                               , title :: Maybe Text
                               , message :: Maybe Text
                               , captcha :: Text
                               , password :: Text
                               , dest :: Maybe Int
                               , files :: [Int]
                               , parent :: Int
                               , board :: Text  
                               } deriving Generic

instance FromJSON PostRequest
