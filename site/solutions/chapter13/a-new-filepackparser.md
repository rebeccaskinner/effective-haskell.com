---
chapter: 13
exercise-id: 2
name: A New FilePackParser
summary: "
Summary TBD
"
---

## A New FilePackParser {.problem}

Refactor the `FilePackParser` application that you wrote earlier in this book to
use the following Monad:

```haskell
newtype FilePackParser a = FilePackParser
  { runFilePackParser :: StateT Text (ExceptT Text IO) a }
```

### Hint 1 {.hint}

Some high level hint text

### Hint 2 {.hint}

Some more detailed hint text

### Hint 3 {.hint}

Even more detailed hint text

### Solution {.solution}

A complete solution for the exercise
