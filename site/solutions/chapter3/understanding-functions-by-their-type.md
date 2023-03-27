---
chapter: 3
exercise-id: 2
name: Understanding Functions by Their Type
summary: "
In this exercise you'll practice understanding the behavior of functions based
on their type. You'll look at several different functions and try to figure out
how they'll behave based on their types.
"
---

## Understanding Functions By Their Type {.problem}

The behavior of the following functions from base can be easily predicted based
on their type. Review the type of each of these functions and try to guess at
how they are implemented. Use `ghci` to see if you were right. Are there other
ways you could have implemented them? Why or why not?

- `Data.Tuple.swap :: (a,b) -> (b,a)`
- `concat :: [[a]] -> [a]`
- `id :: a -> a`

### Hints

<div class="hints">

<details>
<summary>Click to reveal</summary>
Remember that, without any other information, you can't create a value for a
general polymorphic type like `a` or `b` since you don't know what type of value
you should create.
</details>
</div>

### Solution

<div class="solution">

<details>
<summary>Click to reveal</summary>

The `swap` function has a straightforward implementation that closely resembles
it's type. Let's start by taking a look at the simplest implementation:

```haskell
swap :: (a,b) -> (b,a)
swap (a,b) = (b,a)
```

In the exercise, you were asked to consider whether or not there were other ways
you could have implemented `swap`. It's trivially true to write this function
using different code. For example, we can use `fst` and `snd` instead of pattern
matching, or use `let` bindings:

```haskell
swap :: (a,b) -> (b,a)
swap input =
  let
    newFirstElem = snd input
    newSecondElem = fst input
  in (newFirstElem, newSecondElem)
```

The question then is: are these two implementations really _different_? They are
obviously implemented in different ways, but in Haskell we prefer to reason
about functions based on their inputs and outputs. This newer version of `swap`
doesn't change the _behavior_ compared to our original implementation, so for
the sake of this exercise, and for the sake of discussion in most cases, we'd
say these are _the same function_.

The next question then is, can we write a version of `swap` that _behaves_
differently? The answer is no, not really. We could make a version of the
function that crashes, or enters some infinite recursion and never returns a
value, but those errors wouldn't arise naturally from any of the obvious
implementations, so we'll ignore them for now.

If you stop to think about it, you can start to understand why. The input to
`swap` is a tuple that contains two values with types `a` and `b`. We have to
return a tuple with two values, whose types are `b` and `a`. We can't return the
tuple elements in their original order, or ignore one element and duplicate the
other, since `a` and `b` are different types. We have to return one of each, and
in the correct order. Since `a` and `b` could be anything, we can't create a new
value for them (what value would we create?). The only way to return a value of
type `a` is to use the one that was given to us. Same for values of type `b`.

</details>

<details>
<summary>Click to reveal</summary>

Like `swap`, there's an obvious definition of `concat` that we can start
with. Since `concat` is part of `Prelude` if you are following along you'll need
to either name your function something else, like `myConcat`, or you'll need to
add this to the top of your source file, after the `module` line:

```haskell
import Prelude hiding (concat)
```

The first place your mind might go when writing `concat` is a manually recursive
version of the function:

```haskell
concat :: [[a]] -> [a]
concat [] = []
concat (x:xs) = x <> concat xs
```

Alternatively, you might choose to use `foldr` to write your version:

```haskell
concat :: [[a]] -> [a]
concat = foldr (<>) []
```

Just like with `swap`, these two versions of `concat` will behave the same way,
so we'll say that for our purposes, they are the same function. Unlike with
`swap`, there are also several _other_ definitions of `concat` that would
typecheck, but could return very different results. Let's look at a couple of
examples.

One easy alternative would be to ignore our input and always return an empty
list:

```haskell
concat :: [[a]] -> [a]
concat _ = []
```

Since lists can be empty, we can construct a value of type `[a]` without needing
to be able to create an arbitrary value of type `a`. Similarly, there are some
operations we can do on lists that we can't do on arbitrary values of type
`a`. For example, we can write a version of `concat` that returns the first
list:

```haskell
concatReturnFirst :: [[a]] -> [a]
concatReturnFirst [] = []
concatReturnFirst (x:_) = x
```

There are some other choices available to us as well, like returning the
concatenated list backwards, or returning the longest sublist:

```haskell
concatReverse :: [[a]] -> [a]
concatReverse = reverse . foldr (<>) []

concatLongest :: [[a]] -> [a]
concatLongest = foldr getLongest []
  where
    getLongest subList currentLongest
      | length subList > length currentLongest = subList
      | otherwise = currentLongest

```

All of these examples return substantially different values than the original
`concat`, even though they have the same type. There's a more subtle type of
difference that we should also consider. In the last part of this exercise we
looked at `swap` and noted that we could have written a version of `swap` that
simply crashed or never returned a value. At the time, we didn't bother to think
about that much, since it was unlikely that we'd run into the problem. With
`concat` it's much more likely. For example, imagine that we implemented
`concat` using `foldl`:

```haskell
concatFoldl :: [[a]] -> [a]
concatFoldl = foldl (<>) []
```

For finite lists, this will work exactly the same as our `foldr` version:

```haskell
λ concat [[1,2],[3,4],[5,6]]
[1,2,3,4,5,6]

λ concatFoldl [[1,2],[3,4],[5,6]]
[1,2,3,4,5,6]
```

If we're working with infinite lists though, only the `foldr` based `concat`
will return a value:

```haskell
λ take 10 . concat $ repeat [1,2,3]
[1,2,3,1,2,3,1,2,3,1]

λ take 10 . concatFoldl $ repeat [1,2,3]
Interrupted.
```
This is one of the more subtle examples of how functions with the same type can
differ in their behavior.

These examples show two different ways that functions with the same type can
behave differently. When we're working a type that is concrete enough to allow
us to construct new values, we have the option of constructing arbitrary values
instead of using the inputs that were provided to us. When we're working with
types that have structure or support operations that might lead to us doing
recursion or pattern matching, then we introduce the possibility of infinite
recursion, partial pattern matches, and generally having functions that behave
differently in various edge cases.
</details>

<details>
<summary>Click to reveal</summary>

The final example we need to evaluate is the `id` function. Like `concat` this
is defined in `Prelude` so you'll need to either name your function something
differently, or add `import Prelude hiding (id)` after your module declaration
if you want to follow along.

As with our earlier solutions, let's start out with the obvious implementation:

```haskell
id :: a -> a
id a = a
```

Like `swap`, we don't have enough information to do any meaningful computation
on the value that's passed in, and there's no obvious implementation that puts
us at risk of accidentally raising an error or infinitely recursing.

Where `id` differs from `swap` is that we have even fewer options for how we
might implement this function. The tuple that was passed into `swap` gave us
some structure, and some choices between pattern matching for calling functions
like `fst` and `snd`. By comparison, `id` gives us zero information about the
input, and so it leaves us with nothing productive we can do other than
returning that value.

We could, of course, create some useless intermediate values, but we couldn't
return them since the type of `id` resticts what we can return. Thanks to lazy
evaluation, any intermediate values that aren't used won't be computed, so they
aren't likely to even change the unobservable behavior of the function.

</details>

</div>
