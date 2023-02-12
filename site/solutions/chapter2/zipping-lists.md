---
chapter: 2
exercise-id: 2
name: Zipping Lists
summary: "
The second exercise in chapter 2
"
---

## Zipping Lists {.problem}

The `zip` function is a special case of a more general
function available in Prelude called `zipWith`.  The
`zipWith` function combines two lists according to a
function.  Consider this implementation of `zip` in terms
of zipWith:

```
λ let zip' = zipWith (,)
λ zip' [1..5] [5,4..1]
[(1,5),(2,4),(3,3),(4,2),(5,1)]
```

Implement the `zipWith` function with and without using
list comprehensions.  Can you implement `zipWith` using
`foldl`?

### Hint 1 {.hint}

Some high level hint text

### Hint 2 {.hint}

Some more detailed hint text

### Hint 3 {.hint}

Even more detailed hint text

### Solution {.solution}

A complete solution for the exercise
