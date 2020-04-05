module Repository.Users (getAllUsers, getUserById) where
import Domain.User
import Domain.Color

bob :: User
bob = User { userId = 1, name = "bob", color = Red }

jenny :: User
jenny = User { userId= 2, name = "jenny", color = Blue }

allUsers :: [User]
allUsers = [bob, jenny]

getAllUsers :: [User]
getAllUsers = allUsers

getUserById :: Int -> Maybe User
getUserById id =
  case foundUsersList of
    [] -> Nothing
    (user:_) -> Just user
  where foundUsersList = (filter ((== id) . userId) allUsers)
