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
The first part of this exercises asks us to find a way to print a binary tree
containing strings. Since we don't have a particular output format, let's start
with a naive printing function and refactor if we're not happy with it:

```haskell
showStringNaive :: BinaryTree String -> String
showStringNaive Leaf = ""
showStringNaive (Branch l a r) =
  leftStr <> "," <> a <> "," <> rightStr
  where
    leftStr = showStringNaive l
    rightStr = showStringNaive r
```

You'll notice that the first thing we do is pattern match on the tree. Since a
`Leaf` doesn't have any value associated with it, we can just return an empty
string. To create a string for a `Branch`, we needto include both the current
value in the branch, as well as the stringified versions of the left and right
subtrees.

Let's load this up in `ghci` and test it out:

```haskell
λ showStringNaive $ Leaf
""

λ showStringNaive $ Branch Leaf "a" Leaf
",a,"

λ showStringNaive $ Branch (Branch Leaf "a" Leaf) "b" (Branch Leaf "c" Leaf)
",a,,b,,c,"
```

Our elements are being printed out in order, which is what we want, but the
extra commas are pretty ugly. Let's see if we can fix that. The first thing
we'll do is to decouple traversing the tree from generating the output. Instead
of doing it all in one pass, we'll add a helper function called
`binaryTreeToList` that will traverse the tree and return a list with all of the
elements in the right order:

```haskell
binaryTreeToList :: BinaryTree a -> [a]
binaryTreeToList Leaf = []
binaryTreeToList (Branch l a r) =
  binaryTreeToList l <> [a] <> binaryTreeToList r
```

You can see that the logic here is more or less identical to the logic we used
for putting the tree together, but we're not trying to actually merge the
strings together. This has the additional benefit that we can ignore the details
about what kind of tree we're dealing with, so we could use this for trees with
data other than just strings.

Next, we need to combine the list of strings into a single string with commas
inbetween each element. There's a function in from `Data.List` in `Prelude` that
can do this for us. It's called `intercalate`:

```haskell
λ import Data.List (intercalate)

λ :t intercalate
intercalate :: [a] -> [[a]] -> [a]

λ intercalate "," ["a","b","c"]
"a,b,c"

λ intercalate "," ["a"]
"a"

λ intercalate "," ["a","b"]
"a,b"
```

We can use this function make our program work as we'd hoped:

```haskell
showStringTree :: BinaryTree String -> String
showStringTree = intercalate "," . binaryTreeToList
```

Let's run this and see how it works:

```haskell
λ showStringTree $ Branch (Branch Leaf "a" Leaf) "b" (Branch Leaf "c" Leaf)
"a,b,c"
```

Much better! This version of our function works just as we'd have
hoped. Unfortunately, we had to rely on a function that wasn't covered in the
chapter to get there. Let's address that by writing our own version of
`intercalate`:

```haskell
intercalate :: [a] -> [[a]] -> [a]
intercalate a (x:y:ys) = x <> a <> intercalate a (y:ys)
intercalate _ (y:_) = y
intercalate _ [] = []
```
You'll notice that this function uses pattern matching a bit differently than
many other examples you've seen. Normally, when we're pattern matching on a
list, we're only pulling out a single element:

```haskell
(x:xs)
```

In `intercalate` we're pattern matching on the first _two_ elements of a
list. We only want to add a new element between pairs of elements- never before
or after a single element. Pattern matching on the first two elements allows us
to easily ensure that our list has two elements. In the second pattern, we only
have a single element list. In that case, we want to return it as is. Similarly,
in the last pattern, we have an empty list and so we can only return an empty
list. We can write this a bit more tersely by combining the last two patterns:

```haskell
intercalate :: [a] -> [[a]] -> [a]
intercalate a (x:y:ys) = x <> a <> intercalate a (y:ys)
intercalate _ rest = concat rest
```

Let's take one last look at all of this together:

```haskell
showStringTree :: BinaryTree String -> String
showStringTree = intercalate "," . binaryTreeToList

intercalate :: [a] -> [[a]] -> [a]
intercalate a (x:y:ys) = x <> a <> intercalate a (y:ys)
intercalate _ rest = concat rest

binaryTreeToList :: BinaryTree a -> [a]
binaryTreeToList Leaf = []
binaryTreeToList (Branch l a r) = binaryTreeToList l <> [a] <> binaryTreeToList r
```

</details>

<details>
<summary>Click to reveal</summary>

The next part of our exercises asks us to add an element to a tree of
`Int`s. Just like the last part of this exercise, we have some flexibility here
to determine for ourselves exactly how we want to handle inserting a
value. Let's go with a fairly simple approach. We won't worry about keeping tree
balanced. The first element that we insert will become the root of our tree. Any
elements we insert that are numerically less than the root will go into the
left side of the tree, and any elements greater tan the root will go into the
right side of the tree. If an element already exists in the tree, we won't
insert it again. Let's take a look at the code:

```haskell
addElementToIntTree :: BinaryTree Int -> Int -> BinaryTree Int
addElementToIntTree tree n =
  case tree of
    Leaf -> Branch Leaf n Leaf
    Branch l a r
      | n > a -> Branch l a (addElementToIntTree r n)
      | n < a -> Branch (addElementToIntTree l n) a r
      | otherwise -> Branch l a r
```

You'll see that the code in this example maps pretty closely to our description
of the algorithm. We start by pattern matching on the tree. If it's empty
(`Leaf`) we create a new root node that contains our value, with empty left and
right subtrees (`Branch Leaf n Leaf`). If we've got a branch, we compare it's
value with our current value. If the value we're insert is bigger, we
recursively insert our value into the right subtree. If it's smaller, we
recursively insert our value into the left subtree. Otherwise, the values must
be the same and so we don't need to do anything.

You'll notice that we're using a wildcard `otherwise` guard in this example
instead of explicitly testing for equality. If we'd written the code with an
explicit equality test, it would have functioned the same way, but you might get
warnings about an incomplete pattern. Although we know that `n` must always be
greater than, less than, or equal to `a`, the compiler doesn't know that and
assumes we might have missed a pattern.

Testing this solution isn't entirely straightforward. In the last part of this
exercise we built a program that would let us display trees full of strings, but
now we're working with trees full of numbers. Let's write couple helper function
to make it easier for us to view the results of inserting a new value:

```haskell
showTree :: BinaryTree Int -> BinaryTree String
showTree Leaf = Leaf
showTree (Branch l a r) = Branch (showTree l) (show a) (showTree r)
```

This `showTree` function will let us convert a tree containing numbers into a
`BinaryTree String` that we can use with the `showStringTree` function that we
wrote earlier. Let's test it out:

```haskell
λ showStringTree . showTree $ Branch (Branch Leaf 1 Leaf) 2 (Branch Leaf 3 Leaf)
"1,2,3"
```

It looks like `showStringTree` is working with a small example list. Let's try
using `addElementToIntTree` to add some numbers to a larger tree, then see if we
get the right output:

```haskell
λ showStringTree . showTree $ addElementToIntTree (addElementToIntTree (addElementToIntTree Leaf 2) 3) 1
"1,2,3"
```

It works! It's also a lot of typing! Let's write another helpfer function- this
time we'll write one that lets us convert a list of numbers into a tree:

```haskell
intTreeFromList :: [Int] -> BinaryTree Int
intTreeFromList = foldl addElementToIntTree Leaf
```

This function uses `foldl` to add all of the elements from a list into the tree,
starting with an empty tree. As you can imagine, we could easily modify this
function to insert elements into an existing tree as well. We're using `foldl`
here since our binary tree operations do not support infinite trees.

Let's give this a shot to see if it works:

```haskell
λ showStringTree . showTree $ intTreeFromList [3,2,1]
"1,2,3"

λ showStringTree . showTree $ intTreeFromList [3,2,1]
"1,2,3"

λ showStringTree . showTree $ intTreeFromList [3,2,1]
"1,2,3"
```

Our function works, and we can see that the order we insert elements
doesn't matter, since `showStringTree` will always print the elements in
order. It is worth keeping in mind that, since we are not rebalancing our tree
on insertion, adding elements that are already in order (or exactly in reverse)
results in an extremely unbalanced tree. This isn't a problem per-se, just a
trade-off we made to keep the solution simple. As you get more experience with
Haskell you can continue to work on this exercise and try to build a version of
your insertion function that will rebalance itself.
</details>

<details>
<summary>Click to reveal</summary>
The last question in this exercise asks us to write a function to find whether
a particular value exists in a tree of numbers. This function ends up being very
similar to our earlier insertion function:

```haskell
doesIntExist :: BinaryTree Int -> Int -> Bool
doesIntExist Leaf _ = False
doesIntExist (Branch l a r) n
  | n > a = doesIntExist r n
  | n < a = doesIntExist l n
  | otherwise = True
```

This algorithm relies on the tree being "well-formed”. That is to say, the tree
should follow the same structure that we used for `addElementToIntTree`. Smaller
elements to the left, larger elements to the right. If the current element that
we're looking at is an empyt `Leaf` then we can be sure that the element doesn't
exist in the tree. If we're looking at a `Branch` whose element matches what
we're looking for, then we've found it. Otherwise, we can look at the left or
right subtree depending on whether the element we're searching for is smaller or
larger than the element at the root of the current tree.

</details>

</div>
