---
chapter: 2
exercise-id: 2
name: Zipping Lists
summary: "
The second exercise in chapter 2
"
---

### Zipping Lists

The `zip` function is a special case of a more general
function available in Prelude called `zipWith`.  The
`zipWith` function combines two lists according to a
function.  Consider this implementation of `zip` in terms
of zipWith:

```haskell
λ let zip' = zipWith (,)
λ zip' [1..5] [5,4..1]
[(1,5),(2,4),(3,3),(4,2),(5,1)]
```

Implement the `zipWith` function with and without using
list comprehensions.  Can you implement `zipWith` using
`foldl`?

### Hints

<div class="hints">
<details>
<summary>Click to reveal</summary>
You can use pattern matching to easily figure out if either of the lists that
you are zipping is empty.
</details>

<details>
<summary>Click to reveal</summary>
The `zip` and `zipWith` functions in `Prelude` always return a list as long as
the _shortest_ input list. If either list is empty, they return an empty
list. Let's look at a couple of examples:

```haskell
λ zip [] [1..100]
[]

λ zip [1..100] []
[]

λ zip [1] [2..100]
[(1,2)]
```
</details>

<details>
<summary>Click to reveal</summary>

Implementing `zipWith` using list comprehensions will be tricky. Remember that
by default a list comprehension will generate every combination of elements:

```haskell
λ [(a,b) | a <- [1,2,3], b <- [1,2,3]]
[(1,1),(1,2),(1,3),(2,1),(2,2),(2,3),(3,1),(3,2),(3,3)]
```

Can you think of any ways to work around this?
</details>
</div>

### Solution

<div class="solution">
<details>
<summary>Click to reveal</summary>

As you might expect, it's possible to implement `zipWith` with manual recursion,
`foldl`, or using a list comprehension. In fact, there are several different
options, all with their own tradeoffs.

Let's start by looking at the manual recursive definition. We'll use pattern
matching to decide whether we have enough data to create a new value. Naively,
we could check if either list is empty, and otherwise return a value:

```haskell
exampleZipWith f [] bs = []
exampleZipWith f as [] = []
exampleZipWith f (a:as) (b:bs) = f a b : exampleZipWith f as bs
```

Although it works, this approach is a bit more verbose than necessary. If either
list is empty, or if both of them are, then we're returning an empty list. We're
only applying `f` when both lists are non-empty. If we put that pattern first,
then we can use a wildcard pattern for all other cases:

```haskell
exampleZipWith f (a:as) (b:bs) = f a b : exampleZipWith f as bs
exampleZipWith _f _as _bs = []
```

Alternatively, you could use a `case` expression to implement this function. The
logic is the same, but we'll use a single implementation of our function:

```haskell
exampleZipWithCase f a b =
  case (a,b) of
    (a':as, b':bs) -> f a' b' : exampleZipWith f as bs
    _ -> []
```

Another option for implementing our function with manual recursion would be to
use a guard. Naively you might want to use `null` and `head` to implement this
function using a guard:

```haskell
exampleGuard f as bs
  | null as || null bs = []
  | otherwise = f (head as) (head bs) : exampleGuard f (tail as) (tail bs)
```

Although technically safe and correct, since we're testing for empty lists
before using the partial `head` function, it's common practice to avoid partial
functions like `head` in general, even when we know them to be safe. In that
case, we can use _pattern guards_ to pattern match inside of a guard
clause:

```haskell
examplePatternGuard f as bs
  | (a:as') <- as, (b:bs') <- bs = f a b : examplePatternGuard f as' bs'
  | otherwise = []
```

You'll notice that the syntax here is the same as the syntax when working with a
list comprehension. We use the left arrow (`<-`) to pattern match on a value. If
any of the patterns fail , then the guard clause fails and we move onto the next
one.

Now that you've seen how to implement `zipWith` using manual recursion, can you
do it using `foldl` or a list comprehension? Try it yourself, or click below to
see the next part of the solution.
</details>

<details>
<summary>Click to reveal</summary>

Now that you've implemented a manually recursive version of `zipWith`, let's
move our attention to a version that uses `foldl`. If we're willing to cheat a
little bit, our implementation is pretty straightforward:

```haskell
zipWithFoldl f as bs = reverse $ foldl applyFunction [] zipped
  where
    zipped = zip as bs
    applyFunction accumulator (a,b) = f a b : accumulator
```

In this solution we're using `zip` to handle combining each pair of elements in
the two lists. Afterwards, we take one trip through the list with `foldl` and
apply `f` to each pair of arguments. You'll notice that we're prepending each
new value to the beginning of our accumulator, and then reversing the whole list
at the end. Doing a single call to `reverse` at the end of our fold lets us
avoid having to update the entire list every time we add a new
element. Alternatively, we could use `foldr` and save ourselves the call to
`reverse`:

```haskell
zipWithFoldr f as bs = foldr applyFunction [] zipped
  where
    zipped = zip as bs
    applyFunction (a,b) accumulator = f a b : accumulator
```

Since the exercise asked us to solve this with `foldl` let's stick with that. If
we don't want to cheat by using `zip`, we can still solve the problem with
`foldl`, but we need to do a bit more work to keep track of our two lists.

Instead of zipping both lists together, then applying our function, we can do
both in a single step:

```haskell
zipWithFoldl' f as bs =
  reverse $ fst results
  where
    results = foldl applyFunction ([], as) bs
    applyFunction (zipped, []) _ = (zipped, [])
    applyFunction (zipped, x:xs) val = (f x val : zipped, xs)
```

This function isn't too different from our original version. We're still
starting with an empty list in our accumulator, and we're still calling `f` for
each item in our fold. What's different is that our accumulator value is now
keeping track of both the new list that we're building up, and also the second
list that we're slowly breaking down. If `as` is shorter than `bs` we'll start
ignoring any new values in the fold and return the list that we were able to
build up as long as we had values in each list.

Once we're finished with the fold, we're left with a tuple that contains both
the new list, as well as any remainder of `as` that we weren't able to
process. We discard the leftover `as` and return the reversed list just like we
did with our earlier `foldl` implementation.

Now that you've seen how to implement `zipWith` using both manual recursion and
`foldl`, you can try to implement it with a list comprehension yourself, or
expand the solution below.
</details>

<details>
<summary>Click to reveal</summary>

Now that you've written `zipWith` using both `foldl` and with manual recursion,
the last task in this exercise is to build a version that uses list
comprehensions. This is the most challenging of the three parts of this problem,
because we're working against the language. This part of the example shows that
just because you can use a feature to do something doesn't mean it's the best
way to do it.

The problem with using a list comprehension to implement `zipWith` is, as you
may recall from the chapter, a list comprehension returns a value for each
pairing of our two lists. That means the naive approach won't work. Let's try it
and see why:

```haskell
exampleZipWithComprehensionBad f as bs = [f a b | a <- as, b <- bs]
```

Let's load this up in `ghci` and compare the behavior of this function with the
real `zipWith`:

```haskell
λ zipWith (,) [1,2] [3,4]
[(1,3),(2,4)]

λ exampleZipWithComprehensionBad (,) [1,2] [3,4]
[(1,3),(1,4),(2,3),(2,4)]
```

As you can see, the real definition of `zipWith` pairs the first element of the
first list with the first element of the second list, and so on, until it
reaches the end of one of the lists. Our list comprehension version pairs the
first element of the first list with _each element_ of the second list, and so
on, until it's gone through every pairing. That's a significantly different
behavior.

So, how can work work around this? Just like with our earlier `foldl` example,
the easiest option is to cheat by using `zip`:

```haskell
exampleZipWithComprehension f as bs = [f a b | (a,b) <- zip as bs]
```

Not only does using `zip` mean that we don't need to worry about one list being
longer than the other, it also combines our two lists so that we don't need to
worry about the fact that list comprehensions don't combine elements the way
we want for `zipWith`.

If we don't want to cheat by using `zip`, then we need to be a bit creative in
how we approach the problem. Using two lists won't work, but how can we get a
single list out of our two lists if we're not combining them with `zip`? Let's
think again about the nature of our problem: We want to combine the *first*
element of `as` with the *first* element of `bs`, then we want to combine the
*second* element of `as` with the *second* element of `bs` and so on until we
reach the end of one of our two lists. Although we have two lists, at each step
we're combining the values at the same index. All we need to do is to step
through the list of indexes.

```haskell
exampleZipWithComprehensionIndex f as bs =
  [f (as !! idx) (bs !! idx) | idx <- [0 .. len - 1]]
  where
    len = min (length as) (length bs)
```

As you can see, moving to an index based approach to using a list comprehension
lets get an implementation that's fairly easy to read, and doesn't require that
we use `zip`.

Thinking about how to implement something like `zipWith` using list
comprehensions is a great way to stretch your mind and think about the different
ways you can apply the features of Haskell creatively, but in practice this
isn't the way we'd normally implement something like this. Although the `(!!)`
operator should be safe in this example since we're checking the length of our
inputs, it's still an unsafe operation. Indexing into the lists repeatedly is
also much less efficient than even using a `foldl` and reversing the
output. Indexing into a list requires that we traverse the whole list up to the
element we want, and so repeated indexing ends up being more work than walking
through the list twice (once for the `foldl` and again for the `reverse`).

</details>
</div>
