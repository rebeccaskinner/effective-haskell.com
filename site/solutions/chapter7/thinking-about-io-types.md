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
It turns out that functions with types like `[IO a] -> IO [a]` are useful and
come up regularly in all sorts of Haskell programs. There are some general
purpose functions that you'll learn about later in the book that will teach you
how to work with functions that are a bit more general than what we'll cover in
this exercise. For now, let's focus on the question at hand. We can write a
function called `sequenceIO` that takes a list of IO actions and returns a
single IO action that returns a list with all of the values.

Let's start with the easiest scenario: If the input is an empty list, we can
just `return` an empty list. The non-empty list case is a bit more complicated,
so we'll leave it `undefined` for now:

```haskell
sequenceIO :: [IO a] -> IO [a]
sequenceIO [] = return []
sequenceIO (x:xs) = undefined
```

In the non-empty case we've pattern matched the first IO action out from our
list. Let's ignore the rest of the list for now, and think about how we can
return a list with just this element, with the right type. We'll need to
evaluate the IO action, so we'll probably want to use `>>=`:

```haskell
sequenceIO :: [IO a] -> IO [a]
sequenceIO [] = return []
sequenceIO (x:xs) = x >>= \x' -> undefined
```

In this example, `x'` will contain the result of evaluating the IO action in
`x`. If we don't care about the rest of the list, we can simply create a new
list that holds this value and `return` it:

```haskell
sequenceIO :: [IO a] -> IO [a]
sequenceIO [] = return []
sequenceIO (x:xs) = x >>= \x' -> return [x]
```

Of course, this will only give us back the first IO action in our list. If we
want everything, we'll need to sequence the rest of the list as well. How should
we do that? We know that `xs` has the type `[IO a]`. If we recursively pass `xs`
to `sequenceIO` we can convert that to a type of `IO [a]`. Let's add that as a
where binding for now, and then think about what to do next:

```haskell
sequenceIO :: [IO a] -> IO [a]
sequenceIO [] = return []
sequenceIO (x:xs) = x >>= \x' -> return [x']
  where rest = sequenceIO xs
```

Next, let's combine the result of our recursive call with `x'`. To do that,
we'll need to get at the value inside of `rest`. We can use `(>>=)` to help us
again. Since it will type check whether we do our recursive call first or last,
let's start with `rest`:

```haskell
sequenceIO :: [IO a] -> IO [a]
sequenceIO [] = return []
sequenceIO (x:xs) = rest >>= \rest' -> x >>= \x' -> return $ x' : rest'
  where rest = sequenceIO xs
```

```haskell
λ> sequenceIO $ map print [1..10]
10
9
8
7
6
5
4
3
2
1
[(),(),(),(),(),(),(),(),(),()]
```

That doesn't look quite right! We would have expected our numbers to be printed
out in ascending order, but in this test we're seeing them printed in reverse
order. It turns out that our choice to put `rest` first has a big impact. When
we pass `rest` into `(>>=)` we have to strictly evaluate the IO action. That
means we're going to

In this example we're creating a new IO action that first runs the IO action at
the head of our list, then recursively runs the IO actions in the remainder of
the list. Finally, it returns the result of our initial IO action cons-ed onto
the result of the IO action that computes the remainder of the list. It type
checks, but let's load up `ghci` to see if it actually works.

```haskell
λ sequenceIO $ map print [1..10]
1
2
3
4
5
6
7
8
9
10
[(),(),(),(),(),(),(),(),(),()]
```

At first glance, this might be a little confusing. Let's try it again with some
intermediate values to help understand what's happening:

```haskell
λ printUpToTen = map print [1..10]
λ :t printUpToTen
printUpToTen :: [IO ()]
λ :t sequenceIO printUpToTen
sequenceIO printUpToTen :: IO [()]
λ sequenceIO printUpToTen
1
2
3
4
5
6
7
8
9
10
[(),(),(),(),(),(),(),(),(),()]
```

That's better! Since all of our calls to `print` are going to return `IO ()`,
after calling `sequenceIO` we're going to be let with a list of plain `()`
values. When we call `sequenceIO` we're first seeing the side effects of each
number being printed, and the final line is the value returned by the function,
which is a list of `()` values. Getting back a list of results is a little
annoying for examples like this one, where we're using IO actions entirely for
their side effects. Let's write a helper function that discards the return
value. We'll follow a common Haskell convention and add an underscore suffix at
the end of our function name to indicate that it ignores its result:

```haskell
sequenceIO_ :: [IO a] -> IO ()
sequenceIO_ actions = sequenceIO actions >> return ()
```

If we test this, you'll see that `ghci` helpfully ignores the final `()` values
and works like we'd expect any sort of `print`-like function to work:

```haskell
λ sequenceIO_ $ map print [1..10]
1
2
3
4
5
6
7
8
9
10
```

So far, so good, but there are a few things worth spending some time on before
we finish up this exercise. First, there's a subtlety in our implementation that
we should take some time to investigate fruther. Second, there's an opportunity
to rewrite our code to be much easier to read using `do` notation.

Remember that whenever we have code like this:

```haskell
a >>= \b -> doSomethingWith b
```

We can rewrite it with `do` notation like this:

```haskell
do
  b <- a
  doSomethingWith b
```

This isn't always more readable, but it can be really helpful in situations like
our implementation of `sequenceIO` where we need to run two IO actions and get
their results before we can move on. Let's give it a try:

```haskell
sequenceIO :: [IO a] -> IO [a]
sequenceIO [] = return []
sequenceIO (x:xs) = do
  x' <- x
  xs' <- sequenceIO xs
  return $ x' : xs'
```

</div>
</div>
</details>
</div>
