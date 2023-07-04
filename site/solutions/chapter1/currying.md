---
chapter: 1
exercise-id: 3
name: Manual Currying
summary: "
This is the third exercise in chapter 1. In this exercise you'll get practice
with function currying and partial application by implementing your own versions
of the curry and uncurry functions.
"
---

## Manual Currying {.problem}
You've learned about how Haskell functions work by taking a single argument. One
way to write a function that takes multiple arguments is to pass in a tuple of
arguments. For example, consider this addition function:

```haskell
uncurriedAddition nums =
  let
    a = fst nums
    b = snd nums
  in a + b
```

Haskell's standard library includes two functions,
`curry` and `uncurry`, that make
it easy for you to convert between functions that take two arguments and
functions that take a tuple. The `curry` function
transforms a function like our `uncurriedAddition`
function and turns it into one that takes two separate arguments. For example:

```
λ addition = curry uncurriedAddition
λ addOne = addition 1
λ addTwo = addition 2
λ addOne 1
2
λ addOne 2
3
λ addOne 3
4
λ addTwo 1
3
λ addTwo 2
4
λ addTwo 3
5
```

Similarly, the `uncurry` function takes a regular
function with two arguments and converts it into a function that accepts a
tuple. For example, using `uncurry` we could have
rewritten `uncurredAddition` like this:

```haskell
uncurriedAddition = uncurry (+)
```

Using what you've learned in this chapter, try implementing your own version of
`curry` and `uncurry`.

Since the standard library already has functions named
`curry` and `uncurry`, you should
select different names for your implementations. After you've written your
versions, compare the behavior to the standard library implementations to ensure
that your versions behave the same way.

### Hints

<div class="hints">

<details>
<summary>Click to reveal</summary>
<div class="details-body-outer">
<div class="details-body">
Remember that functions can be passed around as ordinary arguments. For example,
imagine that we have a function called `addNumbers` that adds two numbers:

```haskell
addNumbers a b = a + b
```

Next, we could write a function `callWithTwoArguments` that takes a function,
and the two arguments that we should call that function with, and returns the
results:

```haskell
callWithTwoArguments f a b = f a b
```

Finally, we can pass `addNumbers` to `callWithTwoArguments` like any other
value. As an example:

```haskell
addThreeAndFive = callWithTwoArguments addNumbers 3 5
```

As you're working on this exercise, remember that you can pass the functions
that you want to curry, or uncurry, just like you'd pass around any other
argument.
</div>
</div>
</details>


<details>
<summary>Click to reveal</summary>
<div class="details-body-outer">
<div class="details-body">
Remember that you can get the first element of a tuple using the `fst` function,
and you can get the second element of a tuple using the `snd` function:

```haskell
λ fst ("hello", "haskell")
"hello"

λ snd ("hello", "haskell")
"haskell"
```
</div>
</div>
</details>

<details>
<summary>Click to reveal</summary>
<div class="details-body-outer">
<div class="details-body">

The built-in `curry` turns a function that takes a tuple into a function that
takes two arguments. Let's look at an example. Imagine that we have a function
that adds two numbers from a tuple:

```haskell
addTuple tuple = fst tuple + snd tuple
```

We can test this out in `ghci` and see that it works just like we'd expect.

```haskell
λ addTuple (1,2)
3
```

If we use `curry`, we can call this like an ordinary function. We can either use
curry and pass arguments all in one call:

```haskell
λ curry addTuple 1 2
3
```

Or we can use `curry` to define a new version of the function that takes two
non-tuple arguments:

```haskell
λ addTwo = curry addTuple
λ addTwo 1 2
3
λ addTwo 3 4
7
```

The `uncurry` function works the same way, but it converts functions in the
other direction. For example:

```haskell
λ uncurry addTwo (1,2)
3
λ addTuple' = uncurry addTwo
λ addTuple' (2,3)
5
```

You can use these examples as you are testing your own implementation.
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
Let's start by defining our own version of `curry` called `exampleCurry`. Our
function will need to take three arguments:

  - `f` is a function that takes in a tuple and returns a value
  - `a` is a value; it's the first value in the tuple we'll pass to `f`
  - `b` is a value; it's the second value in the tuple we'll pass to `f`

It's easiest to write this function as a one-liner:

```haskell
exampleCurry f a b = f (a,b)
```

In this code, we're taking three arguments as input. We're calling our first
argument, the function we want to curry, using a tuple made up of the next two
arguments.

If we use this function in `ghci` we can see it behaves like you'd expect:

```haskell
λ addTuple tuple = fst tuple + snd tuple
λ exampleCurry addTuple 1 2
3
```

Just like with the `curry` function defined for us in `Prelude`, we can use
`exampleCurry` to create a new function:

```haskell
λ addTwo = exampleCurry addTuple
λ addTwo 1 2
3
λ addTwo 2 3
5
```

This might be a bit surprising, since you're still getting used to
Haskell. Remember that Haskell makes it easy for us to do _partial
application_. We could have written `addTwo` without partial application by
taking in the two arguments that we should call the curried function with:

```haskell
λ addTwo a b = exampleCurry addTuple a b
λ addTwo 1 2
3
λ addTwo 2 3
5
```
</div>
</div>
</details>
</div>
