---
chapter: 2
exercise-id: 5
name: Folds and Infinite Lists
summary: "
The final exercise in chapter 2
"
---

### Folds and Infinite Lists

You learned in this chapter that you can only use foldr on infinite lists, but
not foldl. Try to work manually through calling foldl on an infinite list What
happens? What does this tell you about why you can’t use foldl on an infinite
list? Are there any other benefits to foldr when dealing with large but finite
lists?

### Hints

<div class="hints">
<details>
<summary>Click to reveal</summary>

<div class="details-body-outer">
<div class="details-body">
Try stepping through this function manually:

```haskell
findFirstEvenFoldl :: [Int]
findFirstEvenFoldl =
  foldl firstEven [] [1..]
  where
    firstEven result x =
      if even x
      then [x]
      else result
```
</div>
</div>
</details>

<details>
<summary>Click to reveal</summary>

<div class="details-body-outer">
<div class="details-body">
We can use `foldr` with inifite lists because we are able to stop processing
without trying to consume the entire list. Is this applicable with large but
finite lists?
</div>
</div>
</details>

### Solution

<div class="solution">
<details>
<summary>Click to reveal</summary>

<div class="details-body-outer">
<div class="details-body">

Let's imagine that we want to write a function that finds the first even element
in a possibly infinite list using `foldl`. We've seen solutions to similar
problems using `foldr` before, but let's review before we move onto
`foldl`. We'll start by creating our own version of `foldr` for reference, and
then writing `findFirstEvenFoldr` to look for the first even number in an
infinite list:

```haskell
foldr f acc [] = acc
foldr f acc (x:xs) = f x $ foldr f acc xs

findFirstEvenFoldr =
  foldr firstEven [] [1..]
  where
    firstEven x result
      | even x = [x]
      | otherwise = result
```

As expected, if we run this in `ghci` we can see that it gives us exactly what
we'd expect:

```haskell
λ findFirstEvenFoldr
[2]
```
First, let's inline `foldr` and make the arguments to the function explicit so
that we can step through it more easily. We'll drop the part of the code that
checks for an empty list to keep things more readable. Since we're dealing with
an infinite list, we don't need to worry about running out of elements.

```haskell
findFirstEvenFoldr acc (x:xs) =
  firstEven x $ findFirstEvenFoldr acc xs
  where
    firstEven x result
      | even x = [x]
      | otherwise = result
```

Next, let's apply `[1..]` to `findFirstEvenFoldr` and step through it once:

```haskell
findFirstEvenFoldr [] [1..] =
firstEven x $ findFirstEvenFoldr acc xs
  where
    firstEven x result
      | even x = [x]
      | otherwise = result

-- pattern match on [1..]
findFirstEvenFoldr [] (1 : [2..]) =
  firstEven x $ findFirstEvenFoldr acc xs
  where
    firstEven x result
      | even x = [x]
      | otherwise = result

-- replace x with 1 and xs with [2..]
findFirstEvenFoldr [] (1 : [2..]) =
  firstEven 1 $ findFirstEvenFoldr acc [2..]
  where
    firstEven 1 result
      | even 1 = [1]
      | otherwise = result

-- expand result
findFirstEvenFoldr [] (1 : [2..]) =
  firstEven 1 $ findFirstEvenFoldr acc [2..]
  where
    firstEven 1 result
      | even 1 = [1]
      | otherwise = findFirstEvenFoldr acc [2..]

-- pattern match [2..] and expand the recursive call
findFirstEvenFoldr [] (1 : [2..]) =
  firstEven 1 $ findFirstEvenFoldr acc [2..]
  where
    firstEven 1 result
      | even 1 = [1]
      | otherwise =
          findFirstEvenFoldr acc (2 : [3..]) =
            firstEven 2 $ findFirstEvenFoldr acc [3..]
            where
              firstEven x result
                | even x = [x]
                | otherwise = result

-- Replace x with 2 and xs with [3..]
findFirstEvenFoldr [] (1 : [2..]) =
  firstEven 1 $ findFirstEvenFoldr acc [2..]
  where
    firstEven 1 result
      | even 1 = [1]
      | otherwise =
          findFirstEvenFoldr acc (2 : [3..]) =
            firstEven 2 $ findFirstEvenFoldr acc [3..]
            where
              firstEven 2 result
                | even 2 = [2]
                | otherwise = findFirstEvenFoldr acc [3..]

-- Simplify by removing guards that are false
findFirstEvenFoldr [] (1 : [2..]) =
  firstEven 1 $ findFirstEvenFoldr acc [2..]
  where
    firstEven 1 result =
      findFirstEvenFoldr acc (2 : [3..]) =
        firstEven 2 $ findFirstEvenFoldr acc [3..]
        where
          firstEven 2 result = [2]
```

As you can see when working through this example, the value of our accumulator
comes from the recursive call we make to `foldr`. Since we're only looking at
the accumulator if the current element fails to match our predicate, we
naturally terminate the recursion as soon as we find a value.

Let's see what happens now if we try this with `foldl`:

```haskell
foldl f acc [] = acc
foldl f acc (x:xs) = foldl f (f acc x) xs

findFirstEvenFoldl =
  foldl firstEven [] [1..]
  where
    firstEven result x
      | even x = [x]
      | otherwise = result
```

Just like before, let's drop the empty list case for `foldl`, and inline `foldl`
so that we have a single function we can step through:

```haskell
findFirstEvenFoldl [] (1 : [2..]) =
  findFirstEvenFoldl (firstEven [] 1) [2..]
  where
    firstEven [] 1
      | even 1 = [1]
      | otherwise = []
```

You'll notice once of the key differences between left and right folds in this
first example. When we were using `foldr`, we called `firstEven` and made a
recursive call if (and only if) it needed to look at the accumulator. In this
`foldl` example we're directly returning the result of our recursive call. Let's
step through a couple more times:

```haskell
findFirstEvenFoldl [] (1 : [2..]) =
  findFirstEvenFoldl (firstEven [] 1) [2..]
  where
    firstEven [] 1
      | even 1 = [1]
      | otherwise = []

findFirstEven [] (2 : [3..]) =
  findFirstEvenFoldl (firstEven [] 2) [3..]
  where
    firstEven [] 2
      | even 2 = [2]
      | otherwise = []

findFirstEven [2] (3 : [4..]) =
  findFirstEvenFoldl (firstEven [] 3) [4..]
  where
    firstEven [2] 3
      | even 3 = [3]
      | otherwise = [2]
```

In the last two steps you can see the problem. Even though `firstEven` has found
an even number and returned it, `findFirstEvenFoldl` continues to make recursive
calls.

#### What about large, but finite, lists?

When dealing with large but finite lists, the choice between `foldr` and `foldl`
can be a bit more nuanced. The benefits to `foldlr` still hold: when folding
over a large list using an operation that can short-circuit, `foldr` will tend
to be a better choice because it allows you to stop processing the list as soon
as you have an answer.

If you need to consume the entire list though, `foldl` may be the better
choice. Why? Look back to the examples from the previous solution. Notice how
the examples where we unrolled `foldr` got more and more nested over
time. That's because each time we use the accumulator value (`result` in the
examples above) in `foldr`, we're stuck waiting until we've hit the end of the
list, then working “backwards” applying functions to get the value we need. This
means we're creating a very deep callstack. Haskell is built for functional
programming, and it does better than most languages at dealing with this kind of
recursive call, but it can still end up being expensive in the worst case
scenario.

Our call to `foldl` on the other hand is _tail recursive_. The return value of
`foldl` is either the accumulator value, a direct recursive call. That means we
don't need to increase the depth of our stack. Instead, we can update the
accumulator “as we go” resulting is less memory usage and more effecient code
when we need to use every element in the list.

Unless you have a specific reason to believe otherwise, it's generally better to
use `foldr` because you can benefit from laziness and the compiler can generally
optimize away the performance penalties. If you do know that your code will
benefit from a left fold, be sure to use `foldl'` instead of plain
`foldl`. These functions work identically, but `foldl'` is optimized and should
generally result in using less memory.

</div>
</div>
</details>
</div>
