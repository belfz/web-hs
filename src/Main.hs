{-# LANGUAGE OverloadedStrings #-}
module Main where

import Data.Monoid ((<>))
import Network.HTTP.Types.Status
import Web.Scotty

import Domain.Color
import Domain.User

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
  let searchedUser = (filter ((== id) . userId) allUsers)
  mapResponse searchedUser
    where mapResponse []    = (status notFound404) >>= (\_ -> emptyRes)
          mapResponse (u:_) = (status ok200) >>= (\_ -> json u)

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
