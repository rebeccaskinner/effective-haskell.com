---
chapter: 3
exercise-id: 1
name: Undefined
summary: "
In this exercise you'll expand on what you've learned about `undefined` and
designing programs using types by identifying a common cause of errors and
thinking about how they can be avoided.
"
---

## Undefined {.problem}

Consider a function that takes 3 integers but hasn't been defined:

```haskell
addThree :: Int -> Int -> Int -> Int
```

There are several different ways that you could write a function like this. For
example here are two possible definitions:

```haskell
-- definition 1
addThree = undefined

-- definition 2
addThree a b c = undefined
```

There are many other ways we could use undefined to write a version of
`addThree` that type checks. Why are there so many different versions?

### Hint 1 {.hint}

Some high level hint text

### Hint 2 {.hint}

Some more detailed hint text

### Hint 3 {.hint}

Even more detailed hint text

### Solution {.solution}

A complete solution for the exercise
