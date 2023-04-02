module EffectiveHaskell.Exercises.Chapter5.UserInfo.KnownUsers where
import EffectiveHaskell.Exercises.Chapter5.UserInfo.User

users :: [User a]
users = [george, porter]
  where
    george = makeUser "george" "secret" "gbird2015@example.com" 1000
    porter = makeUser "porter" "hunter2" "woofwoof@example.com" 500
      -- }
    -- george = User
    --   { userName = "george"
    --   , userInternetPoints = 1000
    --   , userPassword = "secret"
    --   , userEmailAddress = "gbird2015@example.com"
    --   }
    -- porter = User
    --   { userName = "porter"
    --   , userInternetPoints = 500
    --   , userPassword = "hunter2"
    --   , userEmailAddress = "woofwoof@example.com"
    --   }
