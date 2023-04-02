module EffectiveHaskell.Exercises.Chapter5.UserInfo.Authentication where
import EffectiveHaskell.Exercises.Chapter5.UserInfo.User
import EffectiveHaskell.Exercises.Chapter5.UserInfo.KnownUsers
import Data.List

data Authenticated
data Unauthenticated

authenticateUser :: User Unauthenticated -> String -> Maybe (User Authenticated)
authenticateUser user password
  | testUserPassword user password = Just $ makeUser name password email points
  | otherwise = Nothing
 where
   name = userName user
   email = userEmailAddress user
   points = userInternetPoints user

-- | Check the list of known users to find a user with the given
-- name. If found, the user is returned in an unauthenticated state.
-- You can authenticate a user with the 'authenticatedUser' function.
lookupUser :: String -> Maybe (User Unauthenticated)
lookupUser name =
  find (\user -> userName user == name) users
