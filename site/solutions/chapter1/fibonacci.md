---
chapter: 1
exercise-id: 2
name: The Fibonacci Sequence
summary: "
The second exercise in chapter 1
"
---

### The Fibonacci Sequence {.problem}

The fibonacci sequence is a sequence of numbers that can be defined
recursively. The first 10 numbers of the fibonacci sequences are:
0,1,1,2,3,5,8,13,21,34. You can calculate any given fibonacci number,
`n`, by adding up the two previous fibonacci numbers.

Write a function that will compute the nth fibonacci number for any given
number, n. You can test your implementation in `ghci` and compare it to the
example:

```
λ fibonacci 0
0
λ fibonacci 1
1
λ fibonacci 5
5
λ fibonacci 10
55
λ fibonacci 25
75025
```
### Hints
<div class="hints">
<details>
<summary>Click to reveal</summary>
There are _two_ cases where the example `fibonacci` function isn't recursive:

  - The 0th fibonacci number is 0
  - The 1st fibonacci number is 1
</details>

<details>
<summary>Click to reveal</summary>
The recursive case of `fibonacci` needs to make _two_ recursive calls, because
it needs to add the next _two_ smallest fibonacci values.
</details>
</div>

### Solution
<div class="solution">
<details>
<summary>Click to reveal</summary>

You can implement the `fibonacci` function using either `if` expressions or
guards. We'll use guards in this solution.

The definition of `fibonacci` is similar to the `factorial` that you should have
created in the previous exercise. Let's take a look:

```haskell
fibonacci n
  | n <= 0 = 0
  | n == 1 = 1
  | otherwise = fibonacci (n - 1) + fibonacci (n - 2)
```

Once again, you'll notice that we're being a bit generous with our smallest base
case. We're checking for numbers _less than or equal to_ zero so that we can
give a sensible answer even if the user passes in a negative number.

In the next base case, we check to see if our value is `1`. If so, we just
return `1`. These two base cases let use get the sequence started with the first
two numbers:

```haskell
fibonacci 0 = 0
fibonacci 1 = 1
```

In the next step, `n` is `2`. This is a recursive case, and we're going to look
back both one step to `n = 1` and two steps to `n = 0`:

```haskell
fibonacci 2 = fibonacci (2 - 1) + fibonacci (2 - 2)
            = fibonacci 1 + fibonacci 0
            = 1 + 1
            = 1
```

Let's follow up one more time, just to make sure we have the pattern down.

```haskell
fibonacci 3 = fibonacci (3 - 1) + fibonacci (3 - 2)
            = fibonacci 2 + fibonacci 1
            = (fibonacci (2 - 1) + fibonacci (2 - 2)) + 1
            = (fibonacci 1 + fibonacci 0) + 1
            = (1 + 0) + 1
            = 1 + 1
            = 2
```

Remember, when you're working with recursive functions, try to walk through the
recursion manually if you're having trouble understanding how the function works.

</details>
</div>
