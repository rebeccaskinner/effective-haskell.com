---
chapter: 7
exercise-id: 1
name: Thinking about IO Types
summary: "
Summary TBD
"
---

## Thinking About IO Types {.problem}

### The Type of Nested IO Actions

Write a function that returns a value of type `IO (IO String)`. What happens if
you try to use `>>=` with that?  What if you want to print the string?

### A Function From Nested IO Actions

Using your function from the previous example, create a function that has the
type signature: `IO (IO a) -> IO a`.

### Lists of IO Actions

Write a function that returns a value of type `[IO a]`, and a second function
with the type `[IO a] -> IO [a]`.  When might you use a function like that?

### Hint 1 {.hint}

Some high level hint text

### Hint 2 {.hint}

Some more detailed hint text

### Hint 3 {.hint}

Even more detailed hint text

### Solution {.solution}

A complete solution for the exercise
