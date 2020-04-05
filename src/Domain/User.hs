{-# LANGUAGE DeriveGeneric #-}
module Domain.User where

import Data.Aeson (FromJSON, ToJSON)
import GHC.Generics
import Domain.Color

data User = User { userId :: Int, name :: String, color :: Color } deriving (Eq, Show, Generic)
instance ToJSON User
instance FromJSON User

bluify :: User -> User
bluify user = User { userId = (userId user), name = (name user), color = Blue }
