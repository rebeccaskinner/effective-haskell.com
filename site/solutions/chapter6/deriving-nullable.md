---
chapter: 6
exercise-id: 3
name: Deriving Nullable
summary: "
Summary TBD
"
---

## Deriving Nullable {.problem}

In the first exercise in this chapter you should have created an instance of
`Nullable` for `Maybe` and list values. There are a few ways that you could have
approached writing these instances, but let's look at some reasonable
definitions you might have used:

```haskell
module DerivingNullable where
import Prelude hiding (null)
import qualified Prelude (null)

class Nullable a where
  isNull :: a -> Bool
  null :: a

instance Nullable [a] where
  isNull = Prelude.null
  null = []

instance Nullable (Maybe a) where
  isNull Nothing = True
  isNull _ = False
  null = Nothing
```

These instances use a fairly intuitive definition of what should be considered
`null`: empty lists are null, as are `Nothing` values. What if we have an
optional list though?

```
λ isNull Nothing
True
λ isNull []
True
λ isNull (Just [])
False
```

In this case it's not clear whether `Just []` should be considered a null value
or not, it depends entirely on the program we are writing. You can even imagine
that we might want different behavior in different parts of the same program.

In this exercise, try to create an API so that a user can make use of `deriving
via` to create `Nullable` instances of their own types. A user should be able to
decide whether `Just []` should be considered a null value or not by selecting
which type they derive their instance from.


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

</details>
</div>
