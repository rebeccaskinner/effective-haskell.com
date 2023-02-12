---
chapter: 4
exercise-id: 1
name: Planting Trees
summary: "
In this exercise you'll get hands-on experience defining your own data types by
creating your own binary tree implementation and writing several functions to
support operations on those trees.
"
---

## Planting Trees {.problem}

Consider a binary tree with a type:

```haskell
data BinaryTree a = Leaf | Branch (BinaryTree a) a (BinaryTree a)
```

Write the definition of the binary tree type, and then add the following
functions:

```haskell
-- Turn a binary tree of strings into a pretty-printed string
showStringTree :: BinaryTree String -> String

-- Add a new integer into a binary tree of integers
addElementToIntTree :: BinaryTree Int -> Int -> BinaryTree Int

-- Check to see if an int value exists in a binary tree of ints
doesIntExist :: BinaryTree Int -> Int -> Bool
```

### Hint 1 {.hint}

Some high level hint text

### Hint 2 {.hint}

Some more detailed hint text

### Hint 3 {.hint}

Even more detailed hint text

### Solution {.solution}

A complete solution for the exercise
