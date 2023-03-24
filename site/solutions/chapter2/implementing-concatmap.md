---
chapter: 2
exercise-id: 3
name: Implementing concatMap
summary: "
The third exercise in chapter 2
"
---

### Implementing concatMap {.problem}

The `concat` function joins a list of lists into a single
list:

```
concat [[1,2,3],[4,5,6]]
[1,2,3,4,5,6]
```

Prelude provides a function named `concatMap` that can be
naively implemented by composing `concat` and
`map`:

```haskell
concatMap f = concat . map f
```

Try implementing `concatMap` using
`foldl` and `foldr`.  Which one is
better? Why?

### Hints

<div class="hints">
<details>
<summary>Click to reveal</summary>
Remember that you can concatenate two lists using the `(<>)` operator:

```haskell
λ [1,2,3] <> [4,5,6]
[1,2,3,4,5,6]
```
</details>

<details>
<summary>Click to reveal</summary>

You can concatenate two lists by folding over them using the `(<>)` operator.

```haskell
λ concatFoldl = foldl (<>) []
λ concatFoldl [[1,2],[3,4],[5,6]]
[1,2,3,4,5,6]
```
</details>


<details>
<summary>Click to reveal</summary>
You can map over a list by applying your function and then adding the result to
a new list using `(:)`

```haskell
λ mapFoldr f = foldr (\x acc -> f x : acc) []
λ mapFoldr (+1) [1..10]
[2,3,4,5,6,7,8,9,10,11]
```
</details>
</div>

### Solution

<div class="solution">
<details>
<summary>Click to reveal</summary>
We can write `concatMap` easily using either `foldl` or `foldr`. Let's take a
look at a `foldr` based version first:

```haskell
concatMapFoldr f = foldr (\x acc -> f x <> acc) []
```

Like the other examples where we've used `foldr`, this version of `concatMap` is
lazy and well-behaved when we need to deal with infinite lists. The most obvious
example of this is directly passing an infinite list to `concatMapFoldr`:

```haskell
λ take 10 $ concatMapFoldr (\x -> [x,x]) [1..]
[1,1,2,2,3,3,4,4,5,5]
```

We can also handle cases where the input is a finite list, but our function
outputs infinite lists that we we want to concatenate together. You'll notice in
this example we're using a new function: `repeat`. This function takes a value
and generates a list of that value repeated indefinitely:

```haskell
-- When we return an infinite list for each finite input element
λ take 10 $ concatMapFoldr (\x -> repeat x) [1,2,3]
[1,1,1,1,1,1,1,1,1,1]
```

As you might expect, we can also handle both cases at the same time. Even with
an infinitely long input, and a function that generates infinitely long lists
from each input, we're still able to get a finite result:

```haskell
-- When we return an infinite list for each of infinite inputs
λ take 10 $ concatMapFoldr (\x -> repeat x) [1..]
[1,1,1,1,1,1,1,1,1,1]
```

We can implement a `foldl` based version of `concatMap` just as easily as our
`foldr` version, we just need to flip around the order of some of our operations:

```haskell
concatMapFoldl f = foldl (\acc x -> acc <> f x) []
```

As you might expect, this version of `concatMap` doesn't do well with infinite
lists. Whether you pass in an finitely list, generate one with the function you
pass in, or both, `concatMapFoldl` will hang and ever complete.

#### So, Which One Is Better?

In most cases, `concatMapFoldr` is the version of this function that we'd want.
If we look at the behavior of the `concatMap` defined for us in `Prelude`
you'll notice it's behavior with infinite lists matches our `foldr` based
implementation:

```haskell
λ take 10 $ concatMap repeat [1..]
[1,1,1,1,1,1,1,1,1,1]
```

</details>
</div>
