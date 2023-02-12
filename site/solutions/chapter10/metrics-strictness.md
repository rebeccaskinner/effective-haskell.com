---
chapter: 10
exercise-id: 3
name: Metrics Strictness
summary: "
Summary TBD
"
---

## Metrics Strictness {.problem}

Consider the `MetricsStore` type that you defined earlier in this chapter:

```haskell
data MetricsStore = MetricsStore
  { successCount :: Int
  , failureCount :: Int
  , callDuration :: Map.Map String Int
  } deriving (Eq, Show)
```
How might you make use of strictness to improve the performance of metrics? Try
writing some metrics collecting functions using several different approaches to
strictness and profiling the results.

### Hint 1 {.hint}

Some high level hint text

### Hint 2 {.hint}

Some more detailed hint text

### Hint 3 {.hint}

Even more detailed hint text

### Solution {.solution}

A complete solution for the exercise
