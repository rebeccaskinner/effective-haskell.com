---
chapter: 4
exercise-id: 2
name: Eval Division by Zero
summary: "
In this exercise you'll use some of what you've learned throughout the chapter
to revisit the `eval` function from your calculator and add error handling.
"
---

## Eval: Division by Zero {.problem}

Write a new version of your `eval` function named `safeEval` that will return an
error if the user tries to divide by zero. It should have the type:

```haskell
safeEval :: Expr -> Either String Int
```

Here's an example of the output you should expect when using `safeEval`:

```
λ> eval $ Lit 10 `Div` Lit 0
*** Exception: divide by zero
λ> safeEval $ Lit 10 `Div` Lit 0
Left "Error: division by zero"
λ> safeEval $ Lit 10 `Div` Lit 10
Right 1
```

Hint: You may need to make quite a few changes to your `eval` function to
complete this exercise, but no changes to your `Expr` type should be necessary,
and you should not need to write any additional functions.


### Hint 1 {.hint}

Some high level hint text

### Hint 2 {.hint}

Some more detailed hint text

### Hint 3 {.hint}

Even more detailed hint text

### Solution {.solution}

A complete solution for the exercise
