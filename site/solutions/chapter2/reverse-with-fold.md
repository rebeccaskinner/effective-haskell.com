---
chapter: 2
exercise-id: 1
name: Reversing A List With Folds
summary: "
The first exercise in chapter 2
"
---

### Reversing a List with Folds

It's possible to easily implement a `reverse` function using folds. Try to
implement a function that will reverse a list using both `foldl` and
`foldr`. Which one is simpler? why? Might one be more efficient than the other?

### Hints

<div class="hints">

<details>
<summary>Click to reveal</summary>

<div class="details-body-outer">
<div class="details-body">
Reversing a list is really just creating a new list one element at a time so
that by the time you're done adding elements, the final list is in the reverse
order. For both of your folds, the initial accumulator value should be an empty list.
</div>
</div>
</details>

<details>
<summary>Click to reveal</summary>

<div class="details-body-outer">
<div class="details-body">

Regardless of the fold you are using, the general shape of the solution will
look something like this:

```haskell
reverseFold = fold insertElem []
```

The definition of `insertElem` will different depending on the
fold you are using.

*Remember*: For a left fold, you'll add new elements to the list from first to
last. For a right fold, you'll be adding elements from last to first.
</div>
</div>
</details>

<details>
<summary>Click to reveal</summary>

<div class="details-body-outer">
<div class="details-body">

Imagine that you want to reverse the list `[1,2,3]`. You could rewrite
thestarting list more explicitly as:

```haskell
1 : 2 : 3 : []
```

The reversed version of that list is `[3,2,1]` or, more explicitly:

```haskell
3 : 2 : 1 : []
```

For a _left fold_ the first call to `insertElem` will pass in the
starting accumulator and the first value of the list:

```haskell
insertElem 1 []
```

The second call will pass in the next value, and the previous accumulator, and
so on. For our list `[1,2,3]` the calls will end up looking like this:

```haskell
insertElem 3 $
  insertElem 2 $
    insertElem 1 []
```

Try to compare that to the shape of our reversed list and see if you can spot a
function you already know that would do that. You might need to consider
reversing the order of arguments of that function.
</div>
</div>
</details>

</div>

### Solution

<div class="solution">
<details>
<summary>Click to reveal</summary>

<div class="details-body-outer">
<div class="details-body">

Reversing a list using a `foldl` can be done by prepending each new element to
the front of the new list. Since `foldl` is left-associative, we'll start with
the first element of our old list. Adding each new element to the beginning of
the reversed list means we'll finish by adding the final element of the original
list to the beginning of the new list. Following the pattern from the earlier
hint, we could a solution like this:

```haskell
reverseLeft = foldl insertElem []
  where insertElem reversed a = a : reversed
```

Alternatively, we can use the `flip` function to make this a bit more
compact. Remember that `flip` just flips the arguments of a function:

```haskell
flip f b a = f a b
```

Our `foldl` version of `insertElem` is just a flipped version of `(:)`, so we
can rewrite our reverse function as:

```haskell
reverseLeft = foldl (flip (:)) []
```

We can also reverse a list using `foldr` but this will be less efficient. Since
`foldr` is right associative, we start adding elements from the end of the input
list. That means each new element we process needs to be added to the end of our
accumulated list:

```haskell
reverseRight = foldr insertElem []
  where
    insertElem a reversed = reversed <> [a]
```

This is less efficient because we have to walk through the entire reversed list
for every item we add, so that we can insert new items at the end.

</div>
</div>
</details>
</div>
