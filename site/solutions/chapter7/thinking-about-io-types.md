---
chapter: 7
exercise-id: 1
name: Thinking about IO Types
summary: "
Summary TBD
"
---

## Thinking About IO Types {.problem}

This problem is presented in three parts. Hints and solutions are provided for
each part individually, so that you can get hints or view the solution to one
problem without seeing the solution to the others.

### The Type of Nested IO Actions

Write a function that returns a value of type `IO (IO String)`. What happens if
you try to use `(>>=)` with that?  What if you want to print the string?

### Hints
<div class="hints">

<details>
<summary>Click to reveal</summary>
<div class="details-body-outer">
<div class="details-body">
Remember the types of `return` and `>>=`:

```haskell
return :: Monad m => a -> m a
(>>=) :: m a -> (a -> m b) -> m b
```
</div>
</div>
</details>

<details>
<summary>Click to reveal</summary>
<div class="details-body-outer">
<div class="details-body">
Keep in mind that when you see type variables like `m a` that `a` can be any
type, including `IO String`.
</div>
</div>
</details>

</div>


### Solution {.solution}
<div class="solution">
<details>
<summary>Click to reveal</summary>
<div class="details-body-outer">
<div class="details-body">

The easiest way to intentionally create a value with the type `IO (IO String)`
is to use `return` twice:

```haskell
doubleIO :: IO (IO String)
doubleIO = return $ return "hello"
```

In practice though, you probably won't create values like this on
purpose. You're more likely to create a value with a type like `IO (IO String)`
on accident by using `return` with a call to a function that already returns an
IO action. For example:

```haskell
returnRead :: IO (IO String)
returnRead = return $ readFile "/tmp/example"
```

However we end up with the value, if we want to use `(>>=)` with a nested IO
action, it's important to realize that we'll only be dealing with the outer
layer of `IO`. To understand what that means, let's take a look at the type of
`(>>=)`:

```haskell
(>>=) :: m a -> (a -> m b) -> m b
```

Next, let's replace the type variables with our own types to help get a better
idea of what's happening. In our case, `m` will become the outer `IO` of our
nested IO action, and `a` will be the inner IO action with the type `IO
String`. If we substitute these types in for the type variables we'll end up
with:

```haskell
(>>=) :: IO (IO String) -> (IO String -> IO b) -> IO b
```

You'll notice that we still have a type variable, `b`. Importantly, that means
we can use `(>>=)` with any function that returns an IO action, not necessarily
only nested IO actions. This is important when we look at the last part of our
question: how should we print a value inside of a nested IO action, like `IO (IO
String)`.

Let's start by looking at one solution, then we'll step back a bit to untangle
how and why it works:

```haskell
printNestedIO :: IO (IO String) -> IO ()
printNestedIO = (>>= (>>= putStrLn))
```

At first glance this is pretty hard to read! The only thing that's apparent is
that we're calling `(>>=)` twice- which isn't entirely unsurprising since we're
dealing with two nested layers of `IO`, but it's not entirely readable either.

Part of the problem is that we've gone a bit too far with making our code
entirely point-free. Let's change this by adding a variable to hold our nested
IO action:

```haskell
printNestedIO :: IO (IO String) -> IO ()
printNestedIO nestedIO = nestedIO >>= (>>= putStrLn)
```

This is a little better, but it's still kind of hard to read. Let's replace
`(>>= putStrLn)` with a helper function:

```haskell
printNestedIO :: IO (IO String) -> IO ()
printNestedIO nestedIO = nestedIO >>= go
  where
    go :: IO String -> IO ()
    go ioString = ioString >>= putStrLn
```

That's more readable! This refactored version of our code helps make it a bit
more apparent what's happening. We can read this version of our code in two
parts. First, the outer part:

```haskell
printNestedIO nestedIO = nestedIO >>= go
```

In this function we're working with the outer IO action. In our call to `(>>=)`
we can fill in the relevant type variables to see this:

```haskell
(>>=) :: m  (a        ) -> (a         -> m  b ) -> m b
       = IO (IO String) -> (IO String -> IO ()) -> IO ()
m :: IO
a :: IO String
m a :: IO (IO String)
b :: ()
m b :: IO ()
```

Going down a level, the helper function `go` is how we handle our inner IO
action. Let's once again fill in the types for our call to `(>>=)`:

```haskell
(>>=) :: m  a      -> (a      -> m  b ) -> m b
       = IO String -> (String -> IO ()) -> IO ()
m :: IO
a :: String
m a :: IO String
b :: ()
m b :: IO ()
```

This inner function does the work of taking our string and printing it out. In
the next section

</div>
</div>
</details>
</div>

## A Function From Nested IO Actions

Using your function from the previous example, create a function that has the
type signature: `IO (IO a) -> IO a`.

### Hints
<div class="hints">

<details>
<summary>Click to reveal</summary>
<div class="details-body-outer">
<div class="details-body">

In the last part of this exercise, you wrote a function to print a value with
the type `IO (IO String)`. Think about how you can generalize this to returning
the value rather than printing it.

</div>
</div>
</details>

### Solution {.solution}
<div class="solution">
<details>
<summary>Click to reveal</summary>
<div class="details-body-outer">
<div class="details-body">

Although this problem might seem puzzling at first, it turns out that the
solution is pretty straightforward, and it will probably look familiar if you've
already solved the previous part of this problem. Let's start solving this
problem by creating a function called `joinIO` and leaving the definition
`undefined`:

```haskell
joinIO :: IO (IO a) -> IO a
joinIO ioAction = undefined
```
From our experience with `IO` so far, we know that `(>>=)` is pretty important,
and we've seen the last exercise that it gives us a way to get to the inner part
of a nested pair of IO actions. It's clear that we'll want pass our nested
`ioAction` into `(>>=)` but what should be on the other side? Let's try adding a
type hole to see if the compiler can help us out:

```haskell
joinIO :: IO (IO a) -> IO a
joinIO ioAction = ioAction >>= _
```

If we run this, the compiler will give us some useful information:

```haskell
src/EffectiveHaskell/Exercises/Chapter7/Join.hs:4:32: error: …
    • Found hole: _ :: IO a -> IO a
      Where: ‘a’ is a rigid type variable bound by
               the type signature for:
                 joinIO :: forall a. IO (IO a) -> IO a
               at /home/rebecca/projects/effective-haskell.com/solution-code/src/EffectiveHaskell/Exercises/Chapter7/Join.hs:3:1-27
    • In the second argument of ‘(>>=)’, namely ‘_’
      In the expression: ioAction >>= _
      In an equation for ‘joinIO’: joinIO ioAction = ioAction >>= _
    • Relevant bindings include
        ioAction :: IO (IO a)
          (bound at /home/rebecca/projects/effective-haskell.com/solution-code/src/EffectiveHaskell/Exercises/Chapter7/Join.hs:4:8)
        joinIO :: IO (IO a) -> IO a
          (bound at /home/rebecca/projects/effective-haskell.com/solution-code/src/EffectiveHaskell/Exercises/Chapter7/Join.hs:4:1)
      Valid hole fits include
        id :: forall a. a -> a
          with id @(IO a)
          (imported from ‘Prelude’ at /home/rebecca/projects/effective-haskell.com/solution-code/src/EffectiveHaskell/Exercises/Chapter7/Join.hs:1:8-47
           (and originally defined in ‘GHC.Base’))
  |
Compilation failed.
```

The important part of this message is that we need to fill the type hole we
created with something of type `IO a -> IO a`. Your first reaction might be to
write a function with this type as a helper. Let's call it `returnInnerIO`:

```haskell
joinIO :: IO (IO a) -> IO a
joinIO ioAction = ioAction >>= returnInnerIO
  where
    returnInnerIO :: IO a -> IO a
    returnInnerIO a = a >>= return a
```

This works just as you'd expect, and we can test it out in ghci:

```haskell
λ :t joinIO (return $ return "hello")
joinIO (return $ return "hello") :: IO String

λ joinIO (return $ return "hello") >>= putStrLn
hello
```

Even though this works, we're doing more work than we need to. For one thing,
the definition of `returnInnerIO` is essentially creating a new IO action that
runs the first IO action and then returns it's value. We can skip all of that
extra work and simply return `a` directly:

```haskell
joinIO :: IO (IO a) -> IO a
joinIO ioAction = ioAction >>= returnInnerIO
  where
    returnInnerIO :: IO a -> IO a
    returnInnerIO a = a
```

This is a bit better, and you can verify in `ghci` that it still works as
expected, but it turns out that this is still an unnecessary amount of work. You
might recognize that `returnInnerIO` is the same as another function you've
already seen:

```haskell
id :: a -> a
id a = a
```

Remember that when you're dealing with polymorphic functions like `id`, they can
work on types like `IO a` just as well as types like `Int` or `String`. Let's do
another refactor of our solution to remove `returnInnerIO` altogether and
replace it with `id`:

```haskell
joinIO :: IO (IO a) -> IO a
joinIO ioAction = ioAction >>= id
```

Once again, you can test this out in`ghci` to validate that it's still working
as expected.

If you're using a linting tool like
[hlint](https://hackage.haskell.org/package/hlint), or an editor with hlint
integration built in, you might notice that there's one more refactor that we
can do. It turns out that we've rewritten a standard library function named
`join`. You'll learn more about `join` in Chapter 9 of Effective Haskell, but if
you want a quick preview, you can see that we can use it in exactly the same way
that we've been using `joinIO`:

```haskell
λ import Control.Monad (join)
λ join (return $ return "hello") >>= putStrLn
hello
```

</div>
</div>
</details>
</div>
## Lists of IO Actions

Write a function that returns a value of type `[IO a]`, and a second function
with the type `[IO a] -> IO [a]`.  When might you use a function like that?

### Hints
<div class="hints">

<details>
<summary>Click to reveal</summary>
<div class="details-body-outer">
<div class="details-body">
Remember, you can make recursive calls inside of IO actions.
</div>
</div>
</details>

<details>
<summary>Click to reveal</summary>
<div class="details-body-outer">
<div class="details-body">
You'll need to evaluate all of the `IO` actions before you can return a list
with the results.
</div>
</div>
</details>

</div>


### Solution {.solution}
<div class="solution">
<details>
<summary>Click to reveal</summary>
<div class="details-body-outer">
<div class="details-body">


</div>
</div>
</details>
</div>
