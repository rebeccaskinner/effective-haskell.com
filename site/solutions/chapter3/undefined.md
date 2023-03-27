---
chapter: 3
exercise-id: 1
name: Undefined
summary: "
In this exercise you'll expand on what you've learned about `undefined` and
designing programs using types by identifying a common cause of errors and
thinking about how they can be avoided.
"
---

## Undefined

Consider a function that takes 3 integers but hasn't been defined:

```haskell
addThree :: Int -> Int -> Int -> Int
```

There are several different ways that you could write a function like this. For
example here are two possible definitions:

```haskell
-- definition 1
addThree = undefined

-- definition 2
addThree a b c = undefined
```

There are many other ways we could use undefined to write a version of
`addThree` that type checks. Why are there so many different versions?

### Hints

<div class="hints">

<details>
<summary>Click to reveal</summary>
Think about all of the ways that you can η-reduce (eta-reduce) your code when
the definition of the function is `undefined` .
</details>
</div>

<details>
<summary>Click to reveal</summary>
You can also use `undefined` multiple times.
</details>
</div>

### Solution

<div class="solution">
<details>
<summary>Click to reveal</summary>

There are four obvious ways that we might write this function using undefined:

```haskell
-- With all three arguments bound to variables
addThree a b c = undefined

-- With the first two arguments bound to variables
addThree a b = undefined

-- With the first argument bound to a variable
addThree a = undefined

-- With no arguments bound to a variable
addThree = undefined
```

All of these implementations assume that we're replacing the entire body of
`addThree` with `undefined`, but we can replace individual parts of the body as
well. For example, we might create a function called `op` that represents some
binary operation that will eventually be defined as `(+)` but for now we leave
it `undefined`:

```haskell
addThree :: Int -> Int -> Int -> Int
addThree a b c = op a (op b c)
  where
    op :: Int -> Int -> Int
    op = undefined
```

Or, we could write a pointfree version of this function, with the `undefined` inline:

```haskell
addThree = (undefined .) . undefined
```

Be careful though! We can also use `undefined` to write functions that compile,
but won't really make sense if we try to define the undefined expressions. For
example, we could write:

```haskell
addThree :: Int -> Int -> Int -> Int
addThree = undefined . undefined
```

Although this will compile, there isn't a reasonable definition we could provide
for `undefined` that would do what we want.
`undefined`. We can address that by factoring the use of `undefined` out into a
function and giving it an explicit type annotation, as we did in our earlier
example using `op`:

```haskell
addThree :: Int -> Int -> Int -> Int
addThree = op . op
  where
    op :: Int -> Int -> Int
    op = undefined
```

Now if we try to compile our program, we'll get a useful error message:

```haskell
Undefined.hs:4:12-13: error: …
    • Couldn't match type ‘Int’ with ‘Int -> Int’
      Expected: Int -> Int -> Int -> Int
        Actual: Int -> Int -> Int
    • In the first argument of ‘(.)’, namely ‘op’
      In the expression: op . op
      In an equation for ‘addThree’:
          addThree
            = op . op
            where
                op :: Int -> Int -> Int
                op = undefined
  |
Undefined.hs:4:17-18: error: …
    • Couldn't match type ‘Int -> Int’ with ‘Int’
      Expected: Int -> Int
        Actual: Int -> Int -> Int
    • Probable cause: ‘op’ is applied to too few arguments
      In the second argument of ‘(.)’, namely ‘op’
      In the expression: op . op
      In an equation for ‘addThree’:
          addThree
            = op . op
            where
                op :: Int -> Int -> Int
                op = undefined
  |
Compilation failed.
```

This gets to the heart of the question "why are there so many different ways to
define an expression using undefined”. Since `undefined` can be used anywhere,
for an expression of any type, it's extremely flexible. You can use `undefined`
almost anywhere, to fill in for almost anything, even things that wouldn't ever
make sense with real code. That's one of the drawbacks of this technique. When
you allow the compiler to infer the type of undefined, you may find that you're
getting a false sense of security when your program compiles. It's useful
frequently enough that you shouldn't necessarily avoid it altogether, but beware
of the drawbacks.

</details>
</div>
