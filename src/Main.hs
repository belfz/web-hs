module Main where

import Data.Monoid ((<>))
import Network.HTTP.Types.Status
import Web.Scotty

import Domain.Color()
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
  nameP <- param "name"
  text ("yo " <> nameP <> "!")

usersRoute :: RouteHandler
usersRoute = do
  json getAllUsers

userRoute :: RouteHandler
userRoute = do
  idP <- param "id"
  let maybeUser = getUserById idP
  mapResponse maybeUser
    where mapResponse Nothing     = (status notFound404) >> emptyRes
          mapResponse (Just user) = (status ok200) >> (json user)

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
