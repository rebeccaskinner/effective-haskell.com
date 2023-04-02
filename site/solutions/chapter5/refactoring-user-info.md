---
chapter: 5
exercise-id: 1
name: Refactoring UserInfo
summary: "
In this exercise you'll have the opportunity to revisit some examples from
earlier in the chapter and refactor them, getting hands-on experience with
structuring your code in the process.
"
---

## Refactoring UserInfo

Refactor the `HaskellBook.Examples.UserInfo` module into smaller modules. Try to
look at different ways that you can separate out the concerns of authentication,
looking up a particular user, and getting information about a user.

### Hints

<div class="hints">
<details>
<summary>Click to reveal</summary>
You may want to rethink the export list from the module. Do you need to export
more things if you break the module up? Are there ways around that?
</details>
</div>

### Solution

<div class="solution">

<details>
<summary>Click to reveal</summary>

If we review the final version of our `UserInfo` module from Chapter 5, you'll
see that our module is doing several different things:

  - Defines the `User` type, and helper functions for interacting with it
  - Creates a known set of users and lets us look up a user by name
  - Provides a method for authenticating a user, given their password

At first glance it would seem like we could easily separate out these three
concerns into three separate modules without any trouble. Looking a bit more
closely though, we have a problem: In our original module, we opted not to
export any functions directly related to the password. This let us ensure that
anyone using our module couldn't misuse the password in some insecure
way. Unfortunately, we need access to be able to supply a password to each of
our known users, and we need to be able to check to see if a password is valid
if we want to build authentication. If we want to refactor the module, we'll
need to make some decisions. Let's look at a few of our options. None of these
choices are necessarily better than the others, they simply offer different
tradeoffs.

#### Don't Restrict the Password Field

Perhaps the simplest approach to refactoring our code is to break our module up
into three new modules that:

  - Define the `User` record
  - Provides a set of default users
  - Handles finding and authenticating users

Let's take a look at this approach to refactoring (Don't worry about the change
in module layout outside of this exercise, this is an artifact of the way the
solutions are written for this site).

First, we'll look at our new `User` module. we're exporting everything from
`User` record now, so there's no need for an explicit export list:

```haskell
module EffectiveHaskell.Exercises.Chapter5.UserInfo.User where

data User isAuthenticated = User
  { userName :: String
  , userPassword :: String
  , userEmailAddress :: String
  , userInternetPoints :: Int
  }
```

Next let's write our `KnownUsers` module, which will container our predefined
user list:

```haskell
module EffectiveHaskell.Exercises.Chapter5.UserInfo.KnownUsers where
import EffectiveHaskell.Exercises.Chapter5.UserInfo.User

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
```

You'll notice that in this example we're defining the list of users, but we
haven't yet written our `lookupUser` function. That will be included in the
`Authentication` module that we'll look at next. The reason for this is that
we've also not yet defined our `Authenticated` and `Unauthenticated`
types. Since we want `lookupUser` to return an unauthenticated user, we'll need
`Unauthenticated` to be in scope when we write `lookupUser`. Let's go ahead and
look at our `Authentication` module now:

```haskell
{-# LANGUAGE RecordWildCards #-}
module EffectiveHaskell.Exercises.Chapter5.UserInfo.Authentication where
import EffectiveHaskell.Exercises.Chapter5.UserInfo.User
import EffectiveHaskell.Exercises.Chapter5.UserInfo.KnownUsers
import Data.List

data Authenticated
data Unauthenticated

authenticateUser :: User Unauthenticated -> String -> Maybe (User Authenticated)
authenticateUser User{..} password
  | userPassword == password = Just User{..}
  | otherwise = Nothing

lookupUser :: String -> Maybe (User Unauthenticated)
lookupUser name =
  find (\user -> userName user == name) users
```

Our `Authentication` module needs to import both the `User` module, for the
definition of the `User` record, and the `KnownUsers` module, for the list of
users.


#### Provide a High-Level Module With Select Re-Exports

Our earlier approach benefitted from requiring very little refactoring, but it
had a couple of drawbacks: The first problem was that, to interact with our
code, we'd often end up needing to import several of our modules. Before
refactoring, someone using our code would only have had to import a single
module. The second problem is that our refactoring required that we export
password information in our `User` module. One way that we can address that is
to create a high level module that selectively re-exports some features out of
the three modules we've just defined. For example, we might want to export
everything except for the `userPassword` field from `User`:

```haskell
module EffectiveHaskell.Exercises.Chapter5.UserInfo
  ( module EffectiveHaskell.Exercises.Chapter5.UserInfo.User
  , module EffectiveHaskell.Exercises.Chapter5.UserInfo.KnownUsers
  , module EffectiveHaskell.Exercises.Chapter5.UserInfo.Authentication
  )
where

import EffectiveHaskell.Exercises.Chapter5.UserInfo.User hiding (userPassword)
import EffectiveHaskell.Exercises.Chapter5.UserInfo.KnownUsers
import EffectiveHaskell.Exercises.Chapter5.UserInfo.Authentication
```

#### Create An Alternative To Directly Accessing Passwords

So far we've looked at solutions that involved compromising on our original
design by exporting the password field for our `User` type. That's not the only
option we have. An alternative would be to add functions to the `User` module to
let us do only the things that we need to do with a password. In our case, we
need to be able to test whether a given password attempt for a user is correct,
and we need to be able to create new users, including setting a password for
them. We can do that by adding two new functions. First, `makeUser` will take
the place of the normal `User` value constructor, and will let us set a
password:

```haskell
makeUser :: String -> String -> String -> Int -> User a
makeUser name passwd email points = User
  { userName = name
  , userPassword = passwd
  , userEmailAddress = email
  , userInternetPoints = points
  }
```

Next, we can write a function called `testUserPassword` that will tell us if a
password attempt for a user is correct or not:

```haskell
testUserPassword :: User a -> String -> Bool
testUserPassword user passwordAttempt =
  passwordAttempt == userPassword user
```

We'll also update the export list for our `User` module export our new
functions, and make sure it doesn't export `userPassword`:

```haskell
module EffectiveHaskell.Exercises.Chapter5.UserInfo.User
  ( User
  , userName
  , userEmailAddress
  , userInternetPoints
  , makeUser
  , testUserPassword
  ) where
```

Once we've finished up our changes to `User` we'll need to update `KnownUsers`
and `Authentication` as well. Let's start by updating `KnownUsers` to use
`makeUser`:

```haskell
module EffectiveHaskell.Exercises.Chapter5.UserInfo.KnownUsers where
import EffectiveHaskell.Exercises.Chapter5.UserInfo.User

users :: [User a]
users = [george, porter]
  where
    george = makeUser "george" "secret" "gbird2015@example.com" 1000
    porter = makeUser "porter" "hunter2" "woofwoof@example.com" 500
```

As you can see, there's not much that needs to change in our known user
definitions. Let's move onto `Authentication` where we'll need to change the
definition of `authenticatedUser` to call `testUserPassword` instead of testing
the password directly. This function doesn't need to be changed much, but we'll
need to stop using record wildcards now that we're no longer exporting the
`User` constructor.

```haskell
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

lookupUser :: String -> Maybe (User Unauthenticated)
lookupUser name =
  find (\user -> userName user == name) users
```

#### Which Approach is Most Common?

All of the approaches we've looked at in this exercise are things you might see
in a real codebase- including the original version that didn't break the larger
module up into smaller components. If you do opt for doing a refactor, choosing
a higher level module that has a more restrictive export list than the lower
level modules it re-exports from is likely the most common and has the best
ergonomics.

</details>
</div>
