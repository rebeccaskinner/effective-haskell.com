---
chapter: 1
exercise-id: 1
name: Factorials
summary: "
In this exercise you'll practice the basics of writing and calling Haskell
functions by creating a recursive implementation of the factorial function.
"
---

### Factorials

The factorial function is a simple function that you can define
recursively. You can compute the factorial of a number, `n`, by
multiplying all of the numbers up to `n`. For example:

```haskell
factorial 5 = 5 * 4 * 3 * 2 * 1 = 120
```

Try implementing your own `factorial` function. You can
test your implementation in `ghci` and compare its output to the example:

```
λ factorial 1
1
λ factorial 3
6
λ factorial 5
120
λ factorial 10
3628800
λ factorial 25
15511210043330985984000000
```

### Hints
<div class="hints">

<details>
<summary>Click to reveal</summary>
<div class="details-body-outer">
<div class="details-body">
Remember that a recursive function needs both a _base case_ that tells the
function to stop calling itself, and a _recursive case_ where the function calls
itself with a smaller value.

The _base case_ for your factorial function is when the number you are
calculating is less than, or equal to, one.
</div>
</div>
</details>

<details>
<summary>Click to reveal</summary>
<div class="details-body-outer">
<div class="details-body">
In the _recursive case_ of your function, you need to multiply the current
number by the _next smallest factorial_.
</div>
</div>
</details>

<details>
<summary>Click to reveal</summary>
<div class="details-body-outer">
<div class="details-body">
You can solve this problem using either `if` expressions or _guards_
</div>
</div>
</details>

<details>
<summary>Click to reveal</summary>
<div class="details-body-outer">
<div class="details-body">
Imagine that we'd use parentheses in the `factorial` example. We might have
written it like this:

```haskell
factorial 5 = 5 * (4 * (3 * (2 * 1)))
```

Think about what function would represent the value inside of each set of
parentheses.
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
You can implement this function using either an `if` expression or a
guard. We'll look at both solutions, starting with the solution that uses an
`if` expression.

The first thing we need to do is write our function and define our base
case. We'll leave the rest of the function undefined for the moment:

```haskell
factorial n =
  if n <= 1
  then 1
  else undefined
```

Technically speaking, *factorial* is only defined for positive numbers. We
haven't yet learned how to prevent users from passing in negative numbers, so
we'll fall back to defensive programming practices and just check for any number
less than or equal to one. If our number is small enough, we'll return the
smallest factorial number: one.

What about for a larger factorial? In that case we need to multiply the current
number by the _next smallest_ factorial. What's the value of the next smallest
factorial? We can find it out using the `factorial` function:

```haskell
factorial n =
  if n <= 1
  then 1
  else n * factorial (n - 1)
```

As you can see in this example, the "next smallest factorial" is our recursive
call to `factorial` with the next smallest value, `n - 1`.

If you prefer guards, you'll notice that the implementation with them is nearly
identical:

```haskell
factorial n
  | n <= 1 = 1
  | otherwise = n * factorial (n - 1)
```
</div>
</div>
</details>
</div>
