---
chapter: 14
exercise-id: 1
name: Handling Correctly Spelled Words
summary: "
Summary TBD
"
---

## Handling Correctly Spelled Words {.problem}

Throughout this chapter you've focused on building a spell checker with the
assumption that most words would need to be compared against the entire
dictionary to find misspellings. In reality, most words in most documents are
spelled correctly, and we could avoid doing a lot of work if we identify
correctly spelled words before calculating the edit distance to potential
candidate corrections.

Use what you've learned in this chapter to update your spell checker to skip
looking for corrections for words that are already spelled correctly.

Hint: Consider using the `hashable` library to generate a unique identifier for
each string in your dictionary.


### Hint 1 {.hint}

Some high level hint text

### Hint 2 {.hint}

Some more detailed hint text

### Hint 3 {.hint}

Even more detailed hint text

### Solution {.solution}

A complete solution for the exercise
