---
chapter: 1
exercise-id: 3
name: Manual Currying
summary: "
The third exercise in chapter 1
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

{:language="haskell"}
~~~
uncurriedAddition = uncurry (+)
~~~

Using what you've learned in this chapter, try implementing your own version of
`curry` and `uncurry`.

Since the standard library already has functions named
`curry` and `uncurry`, you should
select different names for your implementations. After you've written your
versions, compare the behavior to the standard library implementations to ensure
that your versions behave the same way.

### Hint 1 {.hint}

Some high level hint text

### Hint 2 {.hint}

Some more detailed hint text

### Hint 3 {.hint}

Even more detailed hint text

### Solution {.solution}

A complete solution for the exercise
