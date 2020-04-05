{-# LANGUAGE OverloadedStrings #-}
module Main where

import Data.Monoid ((<>))
import Network.HTTP.Types.Status
import Web.Scotty

import Domain.Color
import Domain.User

import Repository.Users

type RouteHandler = ActionM ()

emptyRes :: RouteHandler
emptyRes = text ""

helloRoute :: RouteHandler
helloRoute = do
  text "howdy!"

paramRoute :: RouteHandler
paramRoute = do
  name <- param "name"
  text ("yo " <> name <> "!")

usersRoute :: RouteHandler
usersRoute = do
  json allUsers

userRoute :: RouteHandler
userRoute = do
  id <- param "id"
  let searchedUser = (filter ((== id) . userId) allUsers)
  mapResponse searchedUser
    where mapResponse []    = (status notFound404) >> emptyRes
          mapResponse (u:_) = (status ok200) >> (json u)

bluifyRoute :: RouteHandler
bluifyRoute = do
  user <- jsonData
  json (bluify user)

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
