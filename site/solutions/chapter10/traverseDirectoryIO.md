---
chapter: 10
exercise-id: 1
name: traverseDirectoryIO
summary: "
Summary TBD
"
---

## traverseDirectoryIO {.problem}

Write a new function, `traverseDirectoryIO` that has the type:

```haskell
traverseDirectoryIO :: FilePath -> (FilePath -> IO a) -> IO [a]
```

This function should behave like `traverseDirectory'` but should accept a
function returning an IO action, rather than a value.

### Hint 1 {.hint}

Some high level hint text

### Hint 2 {.hint}

Some more detailed hint text

### Hint 3 {.hint}

Even more detailed hint text

### Solution {.solution}

A complete solution for the exercise
