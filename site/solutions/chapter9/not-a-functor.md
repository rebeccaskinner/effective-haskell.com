---
chapter: 9
exercise-id: 2
name: Not a Functor
summary: "
Summary TBD
"
---

## Not a Functor {.problem}

Imagine that we've created a new type to represent a *sorted* list of
values:

```haskell
{-# LANGUAGE DerivingStrategies #-}
module SortedListFunctor (SortedList, insertSorted) where

data SortedList a = Empty | Cons a (SortedList a)
  deriving stock (Eq, Show)

insertSorted :: Ord a => a -> SortedList a -> SortedList a
insertSorted a Empty = Cons a Empty
insertSorted a (Cons b bs)
  | a >= b = Cons b (insertSorted a bs)
  | otherwise = Cons a (Cons b bs)
```

Although `SortedList` might be useful, it turns out that you can't write a
*correct* instance of `Functor` for a `SortedList`. Try to define `Functor`
yourself and experiment with it's behavior. See if you can figure out why you
can't write a correct instance.

### Hint 1 {.hint}

Some high level hint text

### Hint 2 {.hint}

Some more detailed hint text

### Hint 3 {.hint}

Even more detailed hint text

### Solution {.solution}

A complete solution for the exercise
