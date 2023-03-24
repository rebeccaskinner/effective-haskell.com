---
chapter: 2
exercise-id: 4
name: Maps and Folds
summary: "
The fourth exercise in chapter 2
"
---

### Maps and Folds

Think about the following two lines of code that use `finalmap` and `foldr`.
When might they do the same thing?  When might they differ?  How might that
change if you used `foldl` instead of `foldr`?

```haskell
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

That last result is certainly surprising! Let's try to expand
`composeFandGFoldl` manually and see if we can spot the problem.

```haskell
composeFandGFoldl double add [1,2,3] = foldl (g . f) 0

-- This time we start by doubling our carry value, then adding
foldl (add . double) 0 [1,2,3] =
  if null [1,2,3]
  then 0
  else foldl (add (double 0) 1) [2,3]

-- Simplifying our recursive call to foldl
foldl (add . double) 0 [1,2,3] =
  if null [1,2,3]
  then 0
  else foldl 1 [2,3]

-- Expanding another step
foldl (add . double) 0 [1,2,3] =
  if null [2,3]
  then 1
  else foldl (add (double 1) 2) [3]

-- Expanding again
foldl (add . double) 0 [1,2,3] =
  if null [3]
  then 4
  else foldl (add (double 3) 4) []

-- Expanding one last time
foldl (add . double) 0 [1,2,3] =
  if null []
  then (add (double 3) 4)
  else ...

-- Simplifying
foldl (add . double) 0 [1,2,3] =
  if null []
  then 11
  else ...

= 11
```

As you walk through this code, you can see how the behavior of `foldl` and
`foldr` differs. With `foldl` we're starting with our accumulator value, and
doubling it each time before we add the next element in the list.

#### What About Infinite Lists?

Hopefully by now you are convinced that our two `foldr` examples behave the same
way for some finite length list, but what about cases where the list we're
mapping and folding over is infinite? You've already seen an example in the
chapter of how `foldl` and `foldr` behave differently when working with infinite
lists thanks to laziness; now let's see if `foldr` will behave the same way with
and without `map` when working with infinite lists.

Before we dive into the mechanics, let's try testing the behavior
experimentally. We'll need to a function that will let us get a result
when we're folding over an infinite list. Luckily, we already defined one
earlier in the chapter: `findFirst`. Let's review the definition quickly:

```haskell
findFirst predicate =
  foldr findHelper []
  where
    findHelper listElement maybeFound
      | predicate listElement = [listElement]
      | otherwise = maybeFound
```

You've already seen this work with infinite lists. For example, we can find the
first even number in an infinite list:

```haskell
λ findFirst even [1..]
[2]
```

Let's try running a version of this that won't return at all. First, we'll make
an infinite list of odd numbers. Remember that you can start a range with two
numbers to specify the step size:

```haskell
odds = [1,3..]
```
Next, let's try finding the first even number in our list of odd
numbers. Obviously this won't even return a value, so you can press `control+c`
to stop trying once you are satisfied:

```haskell
λ findFirst even odds
Interrupted.
```

Next, let's add two new versions of `findFirst` that each take another
function. `findFirstCompose` will apply the function using composition, and
`findFirstMap` will use `map`:

```haskell
findFirstCompose predicate g =
  foldr (findHelper . g) []
  where
    findHelper listElement maybeFound
      | predicate listElement = [listElement]
      | otherwise = maybeFound

findFirstMap predicate g =
  foldr findHelper [] . map g
  where
    findHelper listElement maybeFound
      | predicate listElement = [listElement]
      | otherwise = maybeFound
```

Let's load these up into `ghci` and test them by once again looking for even
numbers- but this time we'll turn any even number greater than 9 into an odd
number:

```haskell
λ incrementOverNine n = if n > 9 then n + 1 else n
λ odds = [1,3..]
λ> findFirstCompose even incrementOverNine odds
[12]
λ> findFirstMap even incrementOverNine odds
[12]
```

It appears that, even when working with infinite lists, both the composition and
mapping versions of our function remain identical. In a way this might not be
surprising- we've shown that they should behave the same way when we manually
stepped through each version, but it might still be unintuitive that we can
apply `map` to an _infinite_ list to get back a new _infinite_ list, and then
use `foldr` on that _infinite_ list and get back a regular value.

To understand why that works, let's revisit on of our earlier examples and
refine it a bit. Earlier, when we were looking at how we could manually expand
`\f g -> foldr g 0 . map f` you'll recall that we made a substitution like this:

```haskell
map double [1,2,3] = [double 1, double 2, double 3]
```

This kind of substitution is fine as long as we're working with some small
finite list, but once we're dealing with an infinite list you'll be wondering:
how can we safely replace a call to `map` with a list where every element has
a function applied, when the list itself never ends. The answer is that we
can't, but we don't have to!

Instead of imagining `map` as transforming the entire list at once, we can think
of it in terms of a transformation to the head and tail of a list. In other
words, if we can imagine a list as a head and tail:

```haskell
list = head list : tail list
```

Then we can think of `map f` as applying `f` to the `head` and `map f` to the
tail:

```haskell
map f list = f (head list) : map f (tail list)
```

More concretely:

```haskell
map double [1,2,3] = double 1 : map double [2,3]
                   = double 1 : (double 2 : map double [3])
                   = double 1 : (double 2 : (double 3 : map double []))
                   = double 1 : (double 2 : (double 3 : []))
                   = [double 1, double 2, double 3]
```

The thing that makes this all work is that we never need to evaluate any further
than the head of the list. Let's take a look at how this works with one last
step through a problem. This time, we'll look at using `foldr` and `map` over
our infinite list of `odds`.

```haskell
incrementOverTwo = if n > 2 then n + 1 else n

findFirstMap predicate g =
  foldr findHelper [] . map g
  where
    findHelper listElement maybeFound
      | predicate listElement = [listElement]
      | otherwise = maybeFound

findFirstMap even incrementOverTwo odds =

-- Replace incrementOverTwo and odds with their definitions
findFirstMap even (\n -> if n > 2 then n + 1 else n) [1,3,..] =

-- expand findFirstMap
foldr findHelper [] . map (\n -> if n > 2 then n + 1 else n) [1,3,..] =
  if null (map (\n -> if n > 2 then n + 1 else n) [1,3,..])
  then []
  else findHelper ((\n -> if n > 2 then n + 1 else n) 1) $ foldr findHelper [] $ map (\n -> if n > 2 then n + 1 else n) [3,5..]

-- simplify
foldr findHelper [] . map (\n -> if n > 2 then n + 1 else n) [1,3,..] =
  findHelper (if 1 > 2 then 2 else 1) $ foldr findHelper [] $ map (\n -> if n > 2 then n + 1 else n) [3,5..]

-- simplify
foldr findHelper [] . map (\n -> if n > 2 then n + 1 else n) [1,3,..] =
  findHelper 1 (foldr findHelper [] $ map (\n -> if n > 2 then n + 1 else n) [3,5..])

-- expand findHelper
findHelper 1 (foldr findHelper [] $ map (\n -> if n > 2 then n + 1 else n) [3,5..])
  | even 1 = [1]
  | otherwise = foldr findHelper [] $ map (\n -> if n > 2 then n + 1 else n) [3,5..]

-- 'even 1' is false, so simplify the 'otherwise' branch
foldr findHelper [] $ map (\n -> if n > 2 then n + 1 else n) [3,5..]

-- expand foldr again
if null (map (\n -> if n > 2 then n + 1 else n) [3,5..])
then []
else findHelper ((\n -> if n > 2 then n + 1 else n) 3) $ (map (\n -> if n > 2 then n + 1 else n) [5,7..])

-- simplify
if null (map (\n -> if n > 2 then n + 1 else n) [3,5..])
then []
else findHelper 4 $ map (\n -> if n > 2 then n + 1 else n) [5,7..]

-- expand findHelper again
findHelper 4 $ map (\n -> if n > 2 then n + 1 else n) [5,7..]
  | even 4 = 4
  | otherwise = ... -- we wont' get here
```

As you can see in this example, each time we step through another call to
`foldr` we first apply the function that we're mapping, then we continue with
the fold. Since the recursive call is passed in as the second argument to
`findHelper`, we stop recursively walking through our list as soon as we find a
number that's even after mapping. Thanks to laziness and the way `map` and
`foldr` work, we're able to map and fold over an infinite list and still get out
a normal value.

</details>
</div>
