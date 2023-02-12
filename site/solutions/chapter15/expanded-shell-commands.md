---
chapter: 15
exercise-id: 4
name: Expanded Shell Commands
summary: "
Summary TBD
"
---

## Expanded Shell Commands {.problem}

Expand the GADT based shell command DSL to support a new operation, `wc` that
will call the `wc` program to count the number of lines in a file. After you've
added that, write a function with the type:

```haskell
countLinesInMatchingFiles :: String -> ShellCmd FilePath [(FilePath, Int)]
```

The function should take a glob that can be passed to `grep`, and it should
return a shell command that, given a path to a directory full of files, will
output the name of each file that matched with `grep`, along with the number of
lines in that file. Be sure to only return one output for each file, even if
there are multiple matches within the file.


### Hint 1 {.hint}

Some high level hint text

### Hint 2 {.hint}

Some more detailed hint text

### Hint 3 {.hint}

Even more detailed hint text

### Solution {.solution}

A complete solution for the exercise
