---
chapter: 10
exercise-id: 2
name: Timing Pure Functions
summary: "
Summary TBD
"
---

## Timing Pure Functions {.problem}

The `timeFunction` that you built as you worked through this chapter only
supports timing IO actions. Try writing a version of this function that also
works for pure values, with the type:

```haskell
timePureFunction :: Metrics -> String -> a -> IO a
```
What are the limitations to your implementation function? Are there things that
a user of the function could do to ensure that the timing information was
better?

### Hint 1 {.hint}

Some high level hint text

### Hint 2 {.hint}

Some more detailed hint text

### Hint 3 {.hint}

Even more detailed hint text

### Solution {.solution}

A complete solution for the exercise
