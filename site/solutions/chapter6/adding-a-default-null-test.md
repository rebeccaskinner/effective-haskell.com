---
chapter: 6
exercise-id: 2
name: Adding a Default Null Test
summary: "
Summary TBD
"
---

## Adding a Default Null Test  {.problem}

Add a new `Eq` constraint to the definition of `Nullable`:

```haskell
class Eq a => Nullable a
```
With this change in place, create a default implementation of `isNull`.

### Hints
<div class="hints">

<details>
<summary>Click to reveal</summary>

</details>
</div>

### Solution

<div class="solution">
<details>
<summary>Click to reveal</summary>

This exercise asks us to add an `Eq` constraint to `Nullable` and use that to
allow us to write a default definition of `isNull`. This is a pretty small
change on it's own:

```haskell
class Eq a => Nullable a where
  isNull :: a -> Bool
  isNull = (== null)

  null :: a
```

Unfortunately, changing this definition of our class to add the extra constraint
means that we also need to update most of our instances as well:

```haskell
module EffectiveHaskell.Exercises.Chapter6.DefaultNull where
import Prelude hiding (null)

class Eq a => Nullable a where
  isNull :: a -> Bool
  isNull = (== null)
  null :: a

instance Nullable a => Nullable (Maybe a) where
  isNull Nothing = True
  isNull (Just a) = isNull a
  null = Nothing

instance (Nullable a, Nullable b) => Nullable (a,b) where
  isNull (a,b) = isNull a && isNull b
  null = (null, null)

instance Eq a => Nullable [a] where
  isNull [] = True
  isNull _ = False
  null = []
```

The extra constraint doesn't impact how we're writing our instances, but it does
mean that we won't be able to use the instances in some cases. For example,
before adding the constraint we could use `isNull` to see whether or not we had
an empty list of functions, but functions don't have an `Eq` instance, so we
won't be able to do that anymore.


#### Defaulting with a Helper

#### Using DefaultSignatures

</details>
</div>
