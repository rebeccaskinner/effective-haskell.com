---
chapter: 4
exercise-id: 3
name: Calculator Pretty Printer
summary: "
In this exercise you'll get the opportunity to improve how you think about the
structure of types by building a pretty printer for your calculator. After
you've finished this exercise you'll be able to expand your calculator to
print your calculator expressions in a more human-readable way.
"
---

## Calculator Pretty Printer {.problem}

Write a new function, `prettyPrint`, with the type:

```haskell
prettyPrint :: Expr -> String
```

The function should take any expression and return a human readable string that
shows the calculation as someone might write it themselves.

```
λ putStrLn $ prettyPrint $ Lit 5 `Add` Lit 10
5 + 10 = 15
λ putStrLn $ prettyPrint $ Lit 5 `Add` (Lit 10 `Div` Lit 2)
5 + ( 10 ÷ 2 ) = 10
λ putStrLn $ prettyPrint $ Lit 14 `Mul` (Lit 5 `Add` (Lit 10 `Div` Lit 2))
14 × ( 5 + ( 10 ÷ 2 ) ) = 140
```

### Hint 1 {.hint}

Some high level hint text

### Hint 2 {.hint}

Some more detailed hint text

### Hint 3 {.hint}

Even more detailed hint text

### Solution {.solution}

A complete solution for the exercise
