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
### Hints

<div class="hints">
<details>
<summary>Click to reveal</summary>

Try creating a helper function to turn a `BinaryTree Int` into a `BinaryTree
String` to help you test and debug your functions.

</details>

<details>
<summary>Click to reveal</summary>

You have several different options for how you can print your tree. Displaying
it visually in the terminal as a tree may be pretty difficult right
now. Instead, try thinking about other ways to print out the contents of the
tree.

</details>

<details>
<summary>Click to reveal</summary>

Don't worry about keeping your binary tree balanced. For now, try to insert
elements using the following rules:

  - If the new element is smaller than the root of the tree, insert the element
    on the left
  - If the new element is larger than the root of the tree, insert the element
    on the right
  - If the new element is the same as the root of the tree, do nothing
  - If the tree is empty, insert an element by creating a new root whose left
    and write sides are both empty leaves

</details>

<details>
<summary>Click to reveal</summary>

Use this function to convert a tree of numbers into a tree of strings, so that
you can easily print it out:

```haskell
showTree :: BinaryTree Int -> BinaryTree String
showTree Leaf = Leaf
showTree (Branch l a r) = Branch (showTree l) (show a) (showTree r)
```

</details>

<details>
<summary>Click to reveal</summary>
Try making `showString` print out the elements in order. Here's an example:

```haskell

λ t = addElementToIntTree (addElementToIntTree (addElementToIntTree Leaf 5) 0) 10

λ showStringTree $ showTree t
"0,5,10"

λ showStringTree $ showTree (addElementToIntTree t 3)
"0,3,5,10"

λ showStringTree $ showTree (addElementToIntTree t 12)
"0,5,10,12"

λ showStringTree $ showTree (addElementToIntTree t 2)
"0,2,5,10"

λ showStringTree $ showTree (addElementToIntTree t 9)
"0,5,9,10"
```
</details>

### Solution

<div class="solution">

<details>
<summary>Click to reveal</summary>
</details>

</div>
