---
chapter: 4
exercise-id: 2
name: Eval Division by Zero
summary: "
In this exercise you'll use some of what you've learned throughout the chapter
to revisit the `eval` function from your calculator and add error handling.
"
---

## Eval: Division by Zero {.problem}

Write a new version of your `eval` function named `safeEval` that will return an
error if the user tries to divide by zero. It should have the type:

```haskell
safeEval :: Expr -> Either String Int
```

Here's an example of the output you should expect when using `safeEval`:

```
λ> eval $ Lit 10 `Div` Lit 0
*** Exception: divide by zero
λ> safeEval $ Lit 10 `Div` Lit 0
Left "Error: division by zero"
λ> safeEval $ Lit 10 `Div` Lit 10
Right 1
```

Hint: You may need to make quite a few changes to your `eval` function to
complete this exercise, but no changes to your `Expr` type should be necessary,
and you should not need to write any additional functions.


### Hints

<div class="hints">

<details>
<summary>Click to reveal</summary>

<div class="details-body-outer">
<div class="details-body">
Even though `Div` is the only operation that might fail, you'll need to return
an `Either` value for any operation.
</div>
</div>
</details>

<details>
<summary>Click to reveal</summary>

<div class="details-body-outer">
<div class="details-body">
Remember that if you make a recursive call to `safeEval` you'll need to deal
with the fact that it will return an `Either` instead of an evaluated `Int`.
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
Supporting safe division can be done with a relatively minor refactor of our
existing `eval` code. Let's start by copying our `eval` function and renaming
it, then we can incrementally refactor it to support the behavior we want.

```haskell
safeEval :: Expr -> Int
safeEval expr =
  case expr of
    Lit num -> num
    Add arg1 arg2 -> eval' (+) arg1 arg2
    Sub arg1 arg2 -> eval' (-) arg1 arg2
    Mul arg1 arg2 -> eval' (*) arg1 arg2
    Div arg1 arg2 -> eval' div arg1 arg2
    where
      eval' :: (Int -> Int -> Int) -> Expr -> Expr -> Int
      eval' operator arg1 arg2 =
        operator (safeEval arg1) (safeEval arg2)

```

We now have a `safeEval` function, but it's not all that safe. Let's let the
types help us refactor this code into something that works the way we'd
like. We'll start by changing the type of `safeEval` to return `Either String
Int`:

```haskell
safeEval :: Expr -> Either String Int
```

If we compile the program with just this change you'll see that we're getting
errors now. That makes sense, we've changed the stated type of the function, but
we haven't actually changed any of the values that we're turning. Let's see what
happens if sprinkle a little optimism on our solution and update each of our
case branches to return a `Right` value:

```haskell
safeEval :: Expr -> Either String Int
safeEval expr =
  case expr of
    Lit num -> Right num
    Add arg1 arg2 -> Right $ eval' (+) arg1 arg2
    Sub arg1 arg2 -> Right $ eval' (-) arg1 arg2
    Mul arg1 arg2 -> Right $ eval' (*) arg1 arg2
    Div arg1 arg2 -> Right $ eval' div arg1 arg2
    where
      eval' :: (Int -> Int -> Int) -> Expr -> Expr -> Int
      eval' operator arg1 arg2 =
        operator (safeEval arg1) (safeEval arg2)
```

We're getting closer, but we've still got a compile error. `eval'` is making a
recursive call to `safeEval` for each of our two arguments, but it's still
expecting to get back `Int` instead of the `Either String Int` that we're
returning now. Let's update this function so that we properly handle errors in
the sub-expressions:

```haskell
safeEval :: Expr -> Either String Int
safeEval expr =
  case expr of
    Lit num   -> Right num
    Add arg1 arg2 -> Right $ eval' (+) arg1 arg2
    Sub arg1 arg2 -> Right $ eval' (-) arg1 arg2
    Mul arg1 arg2 -> Right $ eval' (*) arg1 arg2
    Div arg1 arg2 -> Right $ eval' div arg1 arg2
    where
      eval' :: (Int -> Int -> Int) -> Expr -> Expr -> Either String Int
      eval' operator arg1 arg2 =
        case safeEval arg1 of
          Left err -> Left err
          Right a ->
            case safeEval arg2 of
              Left err -> Left err
              Right b -> Right $ operator a b
```

As you can see in ths example, we've had to updated `eval'` to return an `Either
String Int` just like `safeEval`. Since we don't have a sensible default value
to use when we one of the expressions fails, the only option is to return the
error. Unfortunately, now we've got a problem with our original set of
changes. Let's make another refactor to remove the `Right` constructor wrapping
our calls to `safeEval` since we're going to be returning an `Either` value
directly now:

```haskell
safeEval :: Expr -> Either String Int
safeEval expr =
  case expr of
    Lit num   -> Right num
    Add arg1 arg2 -> eval' (+) arg1 arg2
    Sub arg1 arg2 -> eval' (-) arg1 arg2
    Mul arg1 arg2 -> eval' (*) arg1 arg2
    Div arg1 arg2 -> eval' div arg1 arg2
    where
      eval' :: (Int -> Int -> Int) -> Expr -> Expr -> Either String Int
      eval' operator arg1 arg2 =
        case safeEval arg1 of
          Left err -> Left err
          Right a ->
            case safeEval arg2 of
              Left err -> Left err
              Right b -> Right $ operator a b

```

With this set of changes, we finally have a compiling version of our
function. Unfortunately, it's still not actually doing any error handling:

```haskell
λ safeEval $ Lit 10 `Div` Lit 0
Right *** Exception: divide by zero
```
It looks like we're not quite done refactoring afterall. In this version of our
code, we're passing an operator to `eval'`, but the actual operator still can't
deal with failure. If we want to safely handle division by zero, we'll need to
update `eval'` so that the operator it accepts returns an `Either String
Int`. We'll also need to update _all_ of the operators we pass in. In most
cases, we'll be able to always return a `Right` value, but for division we'll
need to check the denominator. Let's take a look at the final version:

```haskell
safeEval :: Expr -> Either String Int
safeEval expr =
  case expr of
    Lit num   -> Right num
    Add arg1 arg2 -> eval' (opHelper (+)) arg1 arg2
    Sub arg1 arg2 -> eval' (opHelper (-)) arg1 arg2
    Mul arg1 arg2 -> eval' (opHelper (*)) arg1 arg2
    Div arg1 arg2 -> eval' safeDiv arg1 arg2
    where
      safeDiv :: Int -> Int -> Either String Int
      safeDiv a b
        | b == 0 = Left "Error: division by zero"
        | otherwise = Right $ a `div` b

      opHelper ::
        (Int -> Int -> Int) ->
        Int ->
        Int ->
        Either String Int
      opHelper op a b = Right $ a `op` b

      eval' ::
        (Int -> Int -> Either String Int) ->
        Expr ->
        Expr ->
        Either String Int
      eval' operator arg1 arg2 =
        case safeEval arg1 of
          Left err -> Left err
          Right a ->
            case safeEval arg2 of
              Left err -> Left err
              Right b -> operator a b

```

In this final version, we've added two new functions. `opHelper` let's us wrap
the operations like multiplication and addition that won't fail, and `safeDiv`
which safely returns a `Left` value when the denominator is zero. Finally, we're
dropped the explicit `Right` constructor in `eval'` since `operator` will now
return an `Either` value. Let's take a look to see this in action:

```haskell
λ safeEval $ (Lit 1 `Div` Lit 0) `Add` (Lit 1 `Mul` Lit 2)
Left "Error: division by zero"

λ safeEval $ (Lit 1 `Div` Lit 1) `Add` (Lit 1 `Mul` Lit 2)
Right 3
```

</div>
</div>
</details>
</div>
