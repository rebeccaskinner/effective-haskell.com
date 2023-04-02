module EffectiveHaskell.Exercises.Chapter5.UserInfo.User
  ( -- * User and Accessor Info
    -- | A 'User' can be some user with an account on a
    -- system. Although users have a password, it should be treated as
    -- opaque in most cases. You can test whether a password is
    -- correct using the 'testUserPassword' function
    User
  , userName
  , userEmailAddress
  , userInternetPoints
    -- * Creating new users

    -- | Although this module supports creating new users with
    -- 'makeUser' you may be more interested in the
    -- 'EffectiveHaskell.Exercises.Chapter5.UserInfo.KnownUsers.users'
    -- collection of known users from
    -- 'EffectiveHaskell.Exercises.Chapter5.UserInfo.KnownUsers'
  , makeUser
    -- * Authentication
    -- | You can build authentication using these functions.
  , testUserPassword
  ) where

-- | A User represents some user on the system. They have a username,
-- password, and contact information, along with some points.
data User isAuthenticated = User
  { userName :: String
    -- ^ The username. We use this to lookup users, and for public identification.
  , userPassword :: String
    -- ^ A users password. Try to keep this private. You can't access
    -- this directly, but you can test whether a password matches or
    -- not with 'testUserPassword'
  , userEmailAddress :: String
    -- ^ A users email address.
  , userInternetPoints :: Int
    -- ^ How many internet points does this user have?
  }

-- | Create a new user.
-- example:
--
-- @
-- makeUser "username1" "hunder2" "user@example.com" 100
-- @
--
makeUser ::
  String -> -- ^ @name@: The username to be assigned to the new user
  String -> -- ^ @passwd@: The password for the new user
  String -> -- ^ @email@: The new user's contact information
  Int ->    -- ^ @points@: Initial number of internet points to assign
  User a
makeUser name passwd email points = User
  { userName = name
  , userPassword = passwd
  , userEmailAddress = email
  , userInternetPoints = points
  }

-- | Given a user and a password, return 'True' if the password matches the given user's password
testUserPassword ::
  User a -> -- ^ @user@: The user whose password should be tested
  String -> -- ^ @passwordAttempt@: A password to attempt
  Bool
testUserPassword user passwordAttempt =
  passwordAttempt == userPassword user
