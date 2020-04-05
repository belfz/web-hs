module Repository.Users (allUsers) where
import Domain.User
import Domain.Color

bob :: User
bob = User { userId = 1, name = "bob", color = Red }

jenny :: User
jenny = User { userId= 2, name = "jenny", color = Blue }

allUsers :: [User]
allUsers = [bob, jenny]
