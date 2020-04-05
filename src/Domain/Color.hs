{-# LANGUAGE DeriveGeneric #-}
module Domain.Color where

import Data.Aeson (FromJSON, ToJSON)
import GHC.Generics

data Color = Red | Green | Blue deriving (Eq, Show, Generic)
instance ToJSON Color
instance FromJSON Color
