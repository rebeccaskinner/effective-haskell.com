---
chapter: 15
exercise-id: 3
name: Type Level Map
summary: "
Summary TBD
"
---

## Type Level Map {.problem}

Write a type family named `Map` that works like the term level `map`
function. It should allow you to apply a function to each element of a type
level list. Next, add a new type or type family so that you can add a number to
each element of a type level list of naturals, as in this example:

```haskell
λ :kind! Map (Add 5) [0,1,2,3,4]
Map (Add 5) [0,1,2,3,4] :: [Natural]
= '[5, 6, 7, 8, 9]
```

### Hint 1 {.hint}

Some high level hint text

### Hint 2 {.hint}

Some more detailed hint text

### Hint 3 {.hint}

Even more detailed hint text

### Solution {.solution}

A complete solution for the exercise
