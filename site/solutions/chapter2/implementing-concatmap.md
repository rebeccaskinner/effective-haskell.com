---
chapter: 2
exercise-id: 3
name: Implementing concatMap
summary: "
The third exercise in chapter 2
"
---

## Implementing concatMap {.problem}


The `concat` function joins a list of lists into a single
list:

```
concat [[1,2,3],[4,5,6]]
[1,2,3,4,5,6]
```

Prelude provides a function named `concatMap` that can be
naively implemented by composing `concat` and
`map`:

```haskell
concatMap f = concat . map f
```

Try implementing `concatMap` using
`foldl` and `foldr`.  Which one is
better? Why?

### Hint 1 {.hint}

Some high level hint text

### Hint 2 {.hint}

Some more detailed hint text

### Hint 3 {.hint}

Even more detailed hint text

### Solution {.solution}

A complete solution for the exercise
