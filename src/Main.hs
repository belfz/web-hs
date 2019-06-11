{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
module Main where

import Data.Monoid ((<>))
import Data.Aeson (FromJSON, ToJSON)
import GHC.Generics
import Web.Scotty
import Network.HTTP.Types.Status

data Color = Red | Green | Blue deriving (Show, Generic)
instance ToJSON Color
instance FromJSON Color

data User = User { userId :: Int, name :: String, color :: Color } deriving (Show, Generic)
instance ToJSON User
instance FromJSON User

bluify :: User -> User
bluify user = User { userId = (userId user), name = (name user), color = Blue }

bob :: User
bob = User { userId = 1, name = "bob", color = Red }

jenny :: User
jenny = User { userId= 2, name = "jenny", color = Blue }

allUsers :: [User]
allUsers = [bob, jenny]

emptyRes :: ActionM ()
emptyRes = text ""

helloRoute :: ActionM ()
helloRoute = do
  text "howdy!"

paramRoute :: ActionM ()
paramRoute = do
  name <- param "name"
  text ("yo " <> name <> "!")

usersRoute :: ActionM ()
usersRoute = do
  json allUsers

userRoute :: ActionM ()
userRoute = do
  id <- param "id"
  let found = (filter (\el -> (userId el) == id) allUsers)
  if length found == 0
    then do
      status notFound404
      emptyRes
    else do
      status ok200
      json (found !! 0)
  -- an equivalent to:    
  -- if length found == 0
  --   then (status notFound404) >>= (\_ -> emptyRes)
  --   else (status ok200) >>= (\_ -> json (found !! 0))

bluifyRoute :: ActionM ()
bluifyRoute = do
  user <- jsonData
  let blueUser = bluify user
  json blueUser

routes :: ScottyM ()
routes = do
  get "/hello" helloRoute
  get "/hi/:name" paramRoute
  get "/users" usersRoute
  get "/users/:id" userRoute
  post "/users/blue" bluifyRoute

main :: IO ()
main = do
  putStrLn "Starting server..."
  scotty 3000 routes
