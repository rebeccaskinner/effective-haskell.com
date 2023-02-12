---
chapter: 3
exercise-id: 3
name: Filling In Type Holes
summary: "
In this exercise you'll get some hands on experience with modifying a larger
example that is missing part of it's implementaiton. Replacing the use of
`undefined` in the example program with the appropriate code will let quickly
make a program pass the test case.
"
---

## Filling In Type Holes {.problem}

Consider the following example code:

```haskell
mapApply :: [a -> b] -> [a] -> [b]
mapApply toApply =
  concatMap (\input -> map ($ input) toApply)

example :: [Int] -> String
example = mapApply undefined
  where
    letters :: [Char]
    letters = ['a'..'z']

    lookupLetter :: Int -> Char
    lookupLetter n = letters !! n

    offsets :: [Int -> Int]
    offsets = [rot13, swap10, mixupVowels]

    rot13 :: Int -> Int
    rot13 n = (n + 13) `rem` 26

    swap10 :: Int -> Int
    swap10 n
      | n <= 10 = n + 10
      | n <= 20 = n - 10
      | otherwise = n

    mixupVowels :: Int -> Int
    mixupVowels n =
      case n of
        0 -> 8
        4 -> 14
        8 -> 20
        14 -> 0
        20 -> 4
        n' -> n'
```

Try to fill in the value of `undefined` so that you get the following output:

```haskell
λ example [5..15]
"spftqgurhvsuwtjxukyblzcmadnbeacfp"
```
Use type holes to help you figure out the type of the value that you'll need to
use.

### Hint 1 {.hint}

Some high level hint text

### Hint 2 {.hint}

Some more detailed hint text

### Hint 3 {.hint}

Even more detailed hint text

### Solution {.solution}

A complete solution for the exercise
