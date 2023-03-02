---
chapter: 2
exercise-id: 5
name: Folds and Infinite Lists
summary: "
The final exercise in chapter 2
"
---

### Folds and Infinite Lists {.problem}

Think about the following two lines of code that use
`map` and `foldr`.  When might
they do the same thing?  When might they differ?  How might that change if you
used `foldl` instead of `foldr`?

```
λ \f g -> foldr g 0 . map f
λ \f g -> foldr (g . f) 0
```
### Hints

<div class="hints">
<details>
<summary>Click to reveal</summary>
Try writing out the definitions of `foldr` and `map` individually. Then, write
out the definition of a function that behaves like `foldr (g . f)`. Are they the
same?
</details>

<details>
<summary>Click to reveal</summary>
If you're having trouble with this exercise, consider returning to it after
finishing chapters 3 and 4. Using the type system can help you get a better
understanding of different functions.
</details>
</div>

### Solution

<div class="solution">
<details>
<summary>Click to reveal</summary>

It turns out that these two examples should always behave exactly the same. In
the first example, we first use `map f` to transform every element of our list,
and then fold over the resulting values. In the second example, each time we'd
apply our fold function we first transform the element we're about to fold,
again using `f`. Since Haskell is pure lazy language, it turns out that these
should always return the same value.

To help make things a little bit more clear, let's walk through a short
example. Imagine that we wanted to double a list of numbers, and then add them
up.

Let's start by giving names to our two different possible functions:

```haskell
composeMapFoldr f g = foldr g 0 . map f
composeFandGFoldr f g = foldr (g . f) 0
```

If we call them in `ghci` we can see that with some casual testing they do in
fact seem to return the same thing:

```haskell
λ double a = a * 2
λ add a b = a + b

λ composeMapFoldr double add [1..10]
110

λ composeFandGFoldr double add [1..10]
110
```

Let's use a smaller list and step through each of these and see how they
work. We'll start with `composeMapFoldr`:

```haskell
composeMapFoldr double add = foldr add 0 . map double

-- If we apply [1,2,3] we'll get
composeMapFoldr double add [1,2,3] = foldr add 0 (map double [1,2,3])

-- Next, let's apply `map double` manually
composeMapFoldr double add [1,2,3] = foldr add 0 [double 1, double 2, double 3]

-- Now we can apply foldr. Refer to the chapter for a definition of foldr
foldr add 0 [double 1, double 2, double 3] =
  if null [double 1, double 2, double 3] -- false
  then 0
  else add (double 1) $ foldr add 0 [double 2, double 3]

-- Expanding another step
foldr add 0 [double 1, double 2, double 3] =
  add (double 1) $
    if null [double 2, double 3] -- false
    then 0
    else add (double 2) $ foldr add 0 [double 3]

-- Expanding one more time
foldr add 0 [double 1, double 2, double 3] =
  add (double 1) $
    add (double 2) $
     add (double 3) $
       if null [] then 0 else ...

-- Simplifying
foldr add 0 [double 1, double 2, double 3] =
  add (double 1) (add (double 2) (add (double 3) 0))

= 12
```

Next, let's do the same exercise with `composeFandGFoldr`:

```haskell
composeFandGFoldr double add [1,2,3] = foldr (add . double) 0 [1,2,3]

-- No need to apply map first, let's go to the first step of foldr
foldr (add . double) 0 [1,2,3] =
  if null [1,2,3]
  then 0
  else add (double 1) $ foldr (add . double) 0 [2,3]

-- Expanding another step
foldr (add . double) 0 [1,2,3] =
  add (double 1) $
    add (double 2) $ foldr (add . double) 0 [3]

-- Expanding one more time
foldr (add . double) 0 [1,2,3] =
  add (double 1) $
    add (double 2) $
      add (double 3) $
        if null [] then 0 else ...

-- Simplifying
foldr add 0 [double 1, double 2, double 3] =
  add (double 1) (add (double 2) (add (double 3) 0))

= 12
```

As you can see, these two functions end up behaving exactly the same.

Unfortunately, the story with `foldl` turns out to be a little bit more
complicated. In theory, thanks to laziness and purity, we ought to be able to
get the same easy substitution with `foldl`, but the the implementation details
mean we have to work a bit harder. Let's start by recreating our two original
functions, this time using `foldl`

```haskell
double a = a * 2
add a b = a + b

composeMapFoldl f g = foldl g 0 . map f
composeFandGFoldl f g = foldl (g . f) 0
```

Like before, let's load these up in `ghci` and see what happens:

```haskell
λ composeMapFoldl double add [1..10]
110

λ composeFandGFoldl double add [1..10]
2036
```

That last result is certainly surprising! Let's dig in and see if we can figure
out what's going on.

</details>
</div>
