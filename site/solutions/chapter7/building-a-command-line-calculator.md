---
chapter: 7
exercise-id: 2
name: Building a Command Line Calculator
summary: "
Summary TBD
"
---

## Building A Command Line Calculator {.problem}

First, Write a program that reads in numbers from the command line and prints
the sum of the provided values.

Next, Modify the program so that the first argument is an operation (`+`, `-`,
or `*`) and performs the supplied operation on the list of numbers.

### Hints
<div class="hints">

<details>
<summary>Click to reveal</summary>
<div class="details-body-outer">
<div class="details-body">
Remember that you can get the command line arguments with the `getArgs` function
from `System.Environment`. You can test programs that use `getArgs` in `ghci`
using the `withArgs` function. For example, let's write a function that prints
out all of the arguments passed in:

```haskell
printArgs :: IO ()
printArgs = getArgs >>= print
```

We can load this up in `ghci` and test it with `withArgs` by passing in a list
of the arguments that `getArgs` should return:

```haskell
λ withArgs ["hello", " ", "world"] printArgs
["hello"," ","world"]
```
</div>
</div>
</details>

<details>
<summary>Click to reveal</summary>
<div class="details-body-outer">
<div class="details-body">
You'll need to use `read` to convert the `String` values that `getArgs` returns
into numbers that you can add together.
</div>
</div>
</details>

<details>
<summary>Click to reveal</summary>
<div class="details-body-outer">
<div class="details-body">
You can pattern match while getting a value from an IO action in `do`
notation. For example, if you wanted to get the first argument, and the
remaining arguments, you can write:

```haskell
(first:rest) <- getArgs
```

Let's look at an example where we print out the first argument, then the
remaining arguments:

```haskell
printArgs :: IO ()
printArgs = do
  (first:rest) <- getArgs
  putStrLn $ "first: " <> first
  putStrLn $ "rest: " <> show rest
```

This works as long as we pass in at least one argument:

```haskell
λ withArgs ["first"] printArgs
first: first
rest: []

λ withArgs ["first", "second", "third"] printArgs
first: first
rest: ["second","third"]
```

But be careful! Like all pattern matching, this is partial and will fail if we
don't pass in any arguments:

```haskell
λ> withArgs [] printArgs
*** Exception: user error (Pattern match failure in 'do' block at ...)
```

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

This exercise asks us to solve two different problems: First, we'd like to add
up all of the numbers passed in as command line arguments, next we'd like to let
the user pick an operation other than addition. We'll take this solution in two
parts. First, let's work on simply adding up the numbers passed in as command
line arguments.

To do this, we'll need to:

  1. get the command line arguments
  2. conver them to numbers
  3. add the list of numbers
  4. print it out

There are a couple of ways to solve this. One choice we'll need to make is
whether we'd like to use `(>>=)` or `do` notation. Let's look at both options,
starting with `(>>=)`. We can write a short point-free implementation of this
function as a one-liner:

```haskell
runBind :: IO ()
runBind = getArgs >>= print . sum . map read
```

This is a fairly idiomatic way to write the function, but if you find point-free
code hard to read, we can refactor this a bit to add some intermediate bindings
that might make it more readable. First, we can factor out the pure code that
transforms the list of strings we get from `getArgs` into the sum that we want
to display:

```haskell
runBind :: IO ()
runBind = getArgs >>= print . sumInputs
  where
    sumInputs inputs = sum $ map read inputs
```

We're still composing `print` with `sumInputs` in this example. If you want to
go another step, we can add another binding for printing out the results of
summing the inputs:

```haskell
runBind :: IO ()
runBind = getArgs >>= showSum
  where
    sumInputs inputs = sum $ map read inputs
    showSum inputs = print $ sumInputs inputs
```

Alternatively, we can stop using `(>>=)` and, instead use `do` notation. Let's
take a look at a similar implementation built around `do`:

```haskell
runDo :: IO ()
runDo = do
  arguments <- getArgs
  let sumOfArgs = sum $ map read arguments
  print sumOfArgs
```

You'll notice in this example that `do` notation tends to encourage a somewhat
more explicit style of programming with more named bindings and less
composition. You can choose whichever style you prefer. For the next part of
this exercise, we'll stick `do` notation.

Building a version of our program that allows the user to select an operation
isn't much more difficult conceptually than supporting only addition, but the
code we need to write will be more complicated due to additional error
handling. Let's take a look at the program, and then walk through how it works:

```haskell
runCalculator :: IO ()
runCalculator = do
  args <- getArgs
  case args of
    [] -> putStrLn argsError
    [_] -> putStrLn argsError
    (op:numStrs) ->
      case getOperation op of
        Just f ->
          let nums = map read numStrs
          in print $ f nums
        Nothing -> putStrLn $ opError op
  where
    argsError =
      "Missing arg(s). Need an operator and at least 1 number"
    opError op =
      op <> " - Unrecognized Operator. Please use one of +,*,-,/"
    getOperation op =
      case op of
        "+" -> Just sum
        "*" -> Just product
        "-" -> Just $ foldl1 (-)
        "/" -> Just $ foldl1 div
        _ -> Nothing
```

In the earlier versions of our program, after getting the arguments we
immediately converted them to numbers, then added them up. Now, we need to deal
with a number of different error cases. The first thing we do in this version of
our program is to check the arguments to make sure that we have gotten at least
two arguments- one operator and at least one number. If we've gotten the right
number of arguments, then we need to check that the first argument is an
operator that we know how to handle. If it is, we return a function that applies
that operator to the remainder of our inputs. Otherwise, we return `Nothing`.

You'll notice that we're using the `foldl1` function for subtraction and
division. This is a useful helper function that behaves similarly to `foldl`,
but it uses the first element of the list as the starting accumulator value.

Once we know that we've got a valid operator, the last step is to convert the
remainder of our inputs to numbers, and then apply our operator. Let's load this
up in `ghci` and see how it works:

```haskell
-- Normal Operations
λ withArgs ["+", "1", "2", "3"] runCalculator
6
λ withArgs ["-", "5", "1", "1"] runCalculator
3
λ withArgs ["*", "2", "3", "5"] runCalculator
30
λ withArgs ["/", "1024", "2", "2", "2"] runCalculator
128

-- Insufficient Arguments
λ withArgs [] runCalculator
Missing arg(s). Need an operator and at least 1 number
λ withArgs ["+"] runCalculator
Missing arg(s). Need an operator and at least 1 number

-- Invalid operator
λ withArgs ["^", "2", "3"] runCalculator
^ - Unrecognized Operator. Please use one of +,*,-,/
```

Later on in the book, you'll learn some ways to handle errors more effectively,
and with less verbosity.

</div>
</div>
</details>

</div>
