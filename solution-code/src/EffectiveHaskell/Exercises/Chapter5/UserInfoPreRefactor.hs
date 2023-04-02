{-# LANGUAGE RecordWildCards #-}
module EffectiveHaskell.Exercises.Chapter5.UserInfoPreRefactor
  ( User
  , lookupUser
  , authenticate
  , getUserName
  , getUserScore
  , getUserEmailAddress
  ) where
import Data.List (find)

data Authenticated
data Unauthenticated

data User isAuthenticated = User
  { userName :: String
  , userInternetPoints :: Int
  , userPassword :: String
  , userEmailAddress :: String
  }

users :: [User a]
users = [george, porter]
  where
    george = User
      { userName = "george"
      , userInternetPoints = 1000
      , userPassword = "secret"
      , userEmailAddress = "gbird2015@example.com"
      }
    porter = User
      { userName = "porter"
      , userInternetPoints = 500
      , userPassword = "hunter2"
      , userEmailAddress = "woofwoof@example.com"
      }

lookupUser :: String -> Maybe (User Unauthenticated)
lookupUser name =
  find (\user -> userName user == name) users

authenticate :: User Unauthenticated -> String -> Maybe (User Authenticated)
authenticate User{..} password
  | userPassword == password = Just User{..}
  | otherwise = Nothing

getUserName :: User isAuthenticated -> String
getUserName = userName

getUserScore :: User isAuthenticated -> Int
getUserScore = userInternetPoints

getUserEmailAddress :: User Authenticated -> String
getUserEmailAddress = userEmailAddress
