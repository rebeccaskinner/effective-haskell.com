---
chapter: 3
exercise-id: 3
name: Filling In Type Holes
summary: "
In this exercise you'll get some hands on experience with modifying a larger
example that is missing part of it's implementaiton. Replacing the use of
`undefined` in the example program with the appropriate code will let quickly
make a program pass the test case.
"
---

## Filling In Type Holes {.problem}

Consider the following example code:

```haskell
mapApply :: [a -> b] -> [a] -> [b]
mapApply toApply =
  concatMap (\input -> map ($ input) toApply)

example :: [Int] -> String
example = mapApply undefined
  where
    letters :: [Char]
    letters = ['a'..'z']

    lookupLetter :: Int -> Char
    lookupLetter n = letters !! n

    offsets :: [Int -> Int]
    offsets = [rot13, swap10, mixupVowels]

    rot13 :: Int -> Int
    rot13 n = (n + 13) `rem` 26

    swap10 :: Int -> Int
    swap10 n
      | n <= 10 = n + 10
      | n <= 20 = n - 10
      | otherwise = n

    mixupVowels :: Int -> Int
    mixupVowels n =
      case n of
        0 -> 8
        4 -> 14
        8 -> 20
        14 -> 0
        20 -> 4
        n' -> n'
```

Try to fill in the value of `undefined` so that you get the following output:

```haskell
λ example [5..15]
"spftqgurhvsuwtjxukyblzcmadnbeacfp"
```
Use type holes to help you figure out the type of the value that you'll need to
use.

### Hints

<div class="hints">

<details>
<summary>Click to reveal</summary>

<div class="details-body-outer">
<div class="details-body">

The goal of this exercise is to understand how to use `undefined` and type holes
to work with code without necessarily needing to understand the implementation
of particular functions. In this exercise, focus on using the functions that
have already been defined for you by looking at their types. You should be able
to complete this exercise without working through the exact algorithm that's
being used to transform the input into an output string.

</div>
</div>
</details>

<details>
<summary>Click to reveal</summary>

<div class="details-body-outer">
<div class="details-body">

If you replace `undefined` with a type hole, the compiler will tell you what
type you should pass to `mapApply`.

</div>
</div>
</details>

<details>
<summary>Click to reveal</summary>

<div class="details-body-outer">
<div class="details-body">

Replacing `undefined` with a type hole shows you that the argument to `mapApply`
should have the type `[Int -> Char]`. Although none of the `where` bindings have
precisely that type, notice that:

  - `offsets` is a list of functions, although they return `Int`
  - `lookupLetter` is the only function that returns a `Char`

</div>
</div>
</details>

<details>
<summary>Click to reveal</summary>

<div class="details-body-outer">
<div class="details-body">

If you're still having trouble, try writing down some of what you know and
adding narrower type holes. In this case, let's see if we can make any progress
by using `map` to transform `offsets` into the type of function we need to pass
into `mapApply`:

```haskell
example = mapApply $ map _ offsets

FillingInTypeHoles.hs:8:26: error: …
    • Found hole: _ :: (Int -> Int) -> Int -> Char
    • In the first argument of ‘map’, namely ‘_’
      In the second argument of ‘($)’, namely ‘map _ offsets’
      In the expression: mapApply $ map _ offsets
    • Relevant bindings include
        lookupLetter :: Int -> Char
          (bound at EffectiveHaskell/Exercises/Chapter3/FillingInTypeHoles.hs:14:5)
        letters :: [Char]
          (bound at EffectiveHaskell/Exercises/Chapter3/FillingInTypeHoles.hs:11:5)
        offsets :: [Int -> Int]
          (bound at EffectiveHaskell/Exercises/Chapter3/FillingInTypeHoles.hs:17:5)
        rot13 :: Int -> Int
          (bound at EffectiveHaskell/Exercises/Chapter3/FillingInTypeHoles.hs:20:5)
        swap10 :: Int -> Int
          (bound at EffectiveHaskell/Exercises/Chapter3/FillingInTypeHoles.hs:23:5)
        mixupVowels :: Int -> Int
          (bound at EffectiveHaskell/Exercises/Chapter3/FillingInTypeHoles.hs:29:5)
        (Some bindings suppressed; use -fmax-relevant-binds=N or -fno-max-relevant-binds)
  |
Compilation failed.
```

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

Although the solution to this exercise is fairly straightforward, but the
process of finding the solution can help us learn how to work with types more
effectively. Let's take a look at the answer, then work through how we got
there:

```haskell
example = mapApply $ map (lookupLetter .) offsets
```

So, how can we figure this out without resorting to working through the actual
implementation details of the algorith? Let's start at the beginning:

```haskell
mapApply :: [a -> b] -> [a] -> [b]
mapApply toApply =
  concatMap (\input -> map ($ input) toApply)

example :: [Int] -> String
example = mapApply undefined
  where
    letters :: [Char]
    letters = ['a'..'z']

    lookupLetter :: Int -> Char
    lookupLetter n = letters !! n

    offsets :: [Int -> Int]
    offsets = [rot13, swap10, mixupVowels]

    rot13 :: Int -> Int
    rot13 n = (n + 13) `rem` 26

    swap10 :: Int -> Int
    swap10 n
      | n <= 10 = n + 10
      | n <= 20 = n - 10
      | otherwise = n

    mixupVowels :: Int -> Int
    mixupVowels n =
      case n of
        0 -> 8
        4 -> 14
        8 -> 20
        14 -> 0
        20 -> 4
        n' -> n'
```

When we have a problem like this, the first thing we can do is to start figuring
out what types we're working with. You'll sometimes hear this process being
called "type tetris” because you're trying to fit the types together.

`mapApply` is a good starting point for this problem. Although it's written as a
polymorphic function, we know that `example` is returning a `String`. That tells
us that, in our call to `mapApply` we're setting `[b]` to `String`. Since
`String` is the same as `[Char]`, we can see that `b` must be `Char`. Let's
re-write the type with this information:

```haskell
mapApply :: [a -> Char] -> [a] -> [Char]
```

We can follow the same process of elimination to figure out what type we'll be
using for `a`, but we're going to have to do a little guessing. In theory we
could ignore list of numbers we're being passed in to `example`, and ignore all
of the functions defined in the `where` clause, but it seems unlikely that we're
intended to ignore all of that and instead pass in some other type. Given the
evidence at hand, let's take a guess and try using `Int` in place of `a`:

```haskell
mapApply :: [Int -> Char] -> [Int] -> [Char]
```

This seems like it could be reasonable. Let's see what the compiler thinks. One
option would be to change the type of `mapApply` and see our program still
compiles. That approach would work for a small example like this, but in a
larger codebase we might not be able to change the type without breaking code
elsewhere, so let's use a type hole at the call site instead:

```haskell
example = mapApply _
```

If we try to compile this, or load it up into `ghci`, we'll see that the
compiler tells us that our guess was exactly right. The first argument to
`mapApply` should be `[Int -> Char]`, meaning `a` must be `Int` and `b` must be
`Char`:

```haskell
FillingInTypeHoles.hs:8:20: error: …
    • Found hole: _ :: [Int -> Char]
    • In the first argument of ‘mapApply’, namely ‘_’
```

Great! So far, so good, but if we look at the functions we have available to us,
none of them have exactly the right type. The closest option that we have is
`offsets`, which has the type `[Int -> Int]`.

We might not be able to pass `offsets` directly, but maybe we can do something
with it to make it fit the right shape. Let's try asking the compiler. We'll
once again use a type hole, but this time we'll use the type hole to figure out
what we should apply `offsets` to so that we can get a function with the type we
need:

```haskell
example = mapApply $ _ offsets
```

In this version of our code, the type hole represents some function that's
accepting `offsets` as an argument and returning a value of the correct
type. Let's compile this version and see what the type of that function should
be:

```haskell
FillingInTypeHoles.hs:9:22: error: …
    • Found hole: _ :: [Int -> Int] -> [Int -> Char]
    • In the second argument of ‘($)’, namely ‘_ offsets’
```

This time the type hole wasn't quite as helpful- it's only restated what we
already knew. Still, we might be able to use this output to help us move
forward. The compiler has reminded us that we need something that can turn a
list of one type into a list of a different type. In other worse, we should try
using `map`:

```haskell
example = mapApply $ map _ offsets
```
We've still got a type hole, but we're slowly narrowing in on the right
code. Let's ask the compiler for help again:

```haskell
FillingInTypeHoles.hs:9:26: error: …
    • Found hole: _ :: (Int -> Int) -> Int -> Char
    • In the first argument of ‘map’, namely ‘_’
      In the second argument of ‘($)’, namely ‘map _ offsets’
      In the expression: mapApply $ map _ offsets
```
Now we're making a little more progress. Our type hole is giving us some more
information about what we should pass to `map`. We need to pass in something
that can take a function with the type `(Int -> Int)` along with some particular
`Int` and return a `Char`. Looking all of our `where` bindings, `lookupLetter`
is the only function that returns a `Char`. The question now is, how should we
use `lookupLetter`? Let's start by adding a new function to our `where` clause:

```haskell
mapFunc :: (Int -> Int) -> Int -> Char
mapFunc f n = undefined
```

We can try to use type holes here, but they aren't like to give us a lot of new
information since we have a good sense of what types we're working with. We just
need to figure out how to put them together.

One option would be to go completely minimal. We have an `Int` value, and we
need a `Char`. We already have a function that does that. Maybe we can ignore
the function we're being passed and just call `lookupLetter` directly?

```haskell
example :: [Int] -> String
example = mapApply $ map mapFunc offsets
  where
    mapFunc :: (Int -> Int) -> Int -> Char
    mapFunc f n = lookupLetter n

    -- ...
```

Our program will compile and run this way, but as you might expect it gives us
the wrong output:

```haskell
λ example [5..15]
"fffggghhhiiijjjkkklllmmmnnnoooppp"
```

We should probably use the function that's being passed in. There's only one way
that we can write this so that it will type check. We need to pass an `Int` into
`f`, and the only `Int` we have is `n`. `f` returns an `Int`, and the only thing
we can do with it is pass it to `lookupLetters`. Let's give it a try:

```haskell
example :: [Int] -> String
example = mapApply $ map mapFunc offsets
  where
    mapFunc :: (Int -> Int) -> Int -> Char
    mapFunc f n = lookupLetter (f n)

    -- ...
```

Let's give this a try:

```haskell
λ example [5..15]
"spftqgurhvsuwtjxukyblzcmadnbeacfp"
```

Success! Following the types, along with a little intuition, has let us finish
implementing our application without needing to delve too deeply into the
implementation details. Before we move on, let's take a moment to do a little
refactoring. Our current version of `mapFunc` is simply passing the output of
one function into another. As you might recall, we can do that using function
composition. We can use eta reduction to remove the explicit `n` parameter at
the same time that we add function composition:

```haskell
mapFunc f = lookupLetter . f
```

Now that we've factored out `n` and we're using function composition, you might
notice that we can eta reduce our function again to remove `f`:

```haskell
mapFunc = (lookupLetter .)
```

At this point, we may as well move the definition of `mapFunc` to it's call
site. That leaves us with a final refactored version that looks like this:

```haskell
mapApply :: [a -> b] -> [a] -> [b]
mapApply toApply =
  concatMap (\input -> map ($ input) toApply)

example :: [Int] -> String
example = mapApply $ map (lookupLetter .) offsets
  where
    letters :: [Char]
    letters = ['a'..'z']

    lookupLetter :: Int -> Char
    lookupLetter n = letters !! n

    offsets :: [Int -> Int]
    offsets = [rot13, swap10, mixupVowels]

    rot13 :: Int -> Int
    rot13 n = (n + 13) `rem` 26

    swap10 :: Int -> Int
    swap10 n
      | n <= 10 = n + 10
      | n <= 20 = n - 10
      | otherwise = n

    mixupVowels :: Int -> Int
    mixupVowels n =
      case n of
        0 -> 8
        4 -> 14
        8 -> 20
        14 -> 0
        20 -> 4
        n' -> n'
```

</div>
</div>
</details>
</div>
