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

## The Type of Nested IO Actions

Write a function that returns a value of type `IO (IO String)`. What happens if
you try to use `(>>=)` with that?  What if you want to print the string?

### Hint 1 {.hint}
<details>
<summary>Click to reveal</summary>
Remember the types of `return` and `>>=`:

```haskell
return :: Monad m => a -> m a
(>>=) :: m a -> (a -> m b) -> m b
```

</details>

### Hint 2 {.hint}
<details>
<summary>Click to reveal</summary>
Keep in mind that when you see type variables like `m a` that `a` can be any
type, including `IO String`.
</details>


### Solution {.solution}
<details>
<summary>Click to reveal</summary>

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

</details>

## A Function From Nested IO Actions

Using your function from the previous example, create a function that has the
type signature: `IO (IO a) -> IO a`.

### Hint 1 {.hint}
<details>
<summary>Click to reveal</summary>
</details>

### Hint 2 {.hint}
<details>
<summary>Click to reveal</summary>
</details>


### Solution {.solution}
<details>
<summary>Click to reveal</summary>
</details>

## Lists of IO Actions

Write a function that returns a value of type `[IO a]`, and a second function
with the type `[IO a] -> IO [a]`.  When might you use a function like that?

### Hint 1 {.hint}
<details>
<summary>Click to reveal</summary>
</details>

### Hint 2 {.hint}
<details>
<summary>Click to reveal</summary>
</details>


### Solution {.solution}
<details>
<summary>Click to reveal</summary>
</details>
