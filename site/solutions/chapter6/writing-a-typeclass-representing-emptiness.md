---
chapter: 6
exercise-id: 1
name: Writing Typeclass Representing Emptiness
summary: "
Summary TBD
"
---

## Writing Typeclass Representing Emptiness {.problem}

Imagine that we wanted to create a typeclass that represents things that can be
“empty” for some definition of empty that will depend on the particular type. In
this exercise we'll call the typeclass `Nullable` and give it two functions:

- `isNull` should return `True` if a value is “empty”
- `null` should return an “empty” value

```haskell
module Nullable where
import Prelude hiding (null)

class Nullable a where
  isNull :: a -> Bool
  null :: a
```

Create instances of this typeclass for:

1. Any `Maybe a` where `a` is `Nullable`
2. Any tuple, `(a,b)` where `a` and `b` are `Nullable`
3. Any list type

### Hint 1 {.hint}

Some high level hint text

### Hint 2 {.hint}

Some more detailed hint text

### Hint 3 {.hint}

Even more detailed hint text

### Solution {.solution}

A complete solution for the exercise
