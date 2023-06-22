---
chapter: 4
exercise-id: 3
name: Calculator Pretty Printer
summary: "
In this exercise you'll get the opportunity to improve how you think about the
structure of types by building a pretty printer for your calculator. After
you've finished this exercise you'll be able to expand your calculator to
print your calculator expressions in a more human-readable way.
"
---

## Calculator Pretty Printer {.problem}

Write a new function, `prettyPrint`, with the type:

```haskell
prettyPrint :: Expr -> String
```

The function should take any expression and return a human readable string that
shows the calculation as someone might write it themselves.

```
λ putStrLn $ prettyPrint $ Lit 5 `Add` Lit 10
5 + 10 = 15
λ putStrLn $ prettyPrint $ Lit 5 `Add` (Lit 10 `Div` Lit 2)
5 + ( 10 ÷ 2 ) = 10
λ putStrLn $ prettyPrint $ Lit 14 `Mul` (Lit 5 `Add` (Lit 10 `Div` Lit 2))
14 × ( 5 + ( 10 ÷ 2 ) ) = 140
```

### Hints

<div class="hints">

<details>
<summary>Click to reveal</summary>

<div class="details-body-outer">
<div class="details-body">
If you are getting stuck on grouping with parentheses, try starting with a
version that doesn't use parentheses at all. Next, refactor your code so that it
adds parentheses around every expression.
</div>
</div>
</details>
</div>

<details>
<summary>Click to reveal</summary>

<div class="details-body-outer">
<div class="details-body">
Try adding a helper function that will prety print an expression and add
parentheses around it afterwards, if the expression isn't a `Lit` value.
</div>
</div>
</details>

### Solution

<div class="solution">

<details>
<summary>Click to reveal</summary>

<div class="details-body-outer">
<div class="details-body">

This exercise is tricky because it requires that we think carefully about how we
carry information along when we traverse a data structure. We're forced to think
about how we can know when to add parentheses to an expression.

When faced with a tricky problem like this, it sometimes helps to defer solving
the tricky part and, instead, to focus on solving the easier parts of the
problem. In our case, we can start by avoiding dealing with parentheses and
instead write a version of the program that doesn't know about order of
operations and prints the expression out naively. Let's take a look at this
naive pretty printer along side our `eval` function

```haskell
eval :: Expr -> Int
eval expr =
  case expr of
    Lit num -> num
    Add arg1 arg2 -> eval' (+) arg1 arg2
    Sub arg1 arg2 -> eval' (-) arg1 arg2
    Mul arg1 arg2 -> eval' (*) arg1 arg2
    Div arg1 arg2 -> eval' div arg1 arg2
    where
      eval' :: (Int -> Int -> Int) -> Expr -> Expr -> Int
      eval' operator arg1 arg2 =
        operator (eval arg1) (eval arg2)

prettyPrintNoParens :: Expr -> String
prettyPrintNoParens expr =
  prettyPrint' expr <> " = " <> show result
  where
    result = eval expr
    prettyPrint' e =
      case e of
        Lit n -> show n
        Sub a b -> prettyOperation " - " a b
        Add a b -> prettyOperation " + " a b
        Mul a b -> prettyOperation " × " a b
        Div a b -> prettyOperation " ÷ " a b

    prettyOperation :: String -> Expr -> Expr -> String
    prettyOperation op a b =
      prettyPrint' a <> op <> prettyPrint' b
```

You'll notice that there's a lot of similarity in our two functions. Both have a
case expression that's matching the particular operation that we're dealing
with, and both pass in the relevant operator while calling out to a helper
function that recursively deals with each sub-expression. The biggest changes
are that we've moved the entire case expression into a helper function, and
we're dealing with strings and using the `(<>)` operator, instead of dealing
with numbers and using function application.

Before we move on, let's run this version of our pretty printer with our input
so we can see it in action:

```haskell
λ putStrLn $ prettyPrintNoParens $ Lit 5 `Add` Lit 10
5 + 10 = 15

λ putStrLn $ prettyPrintNoParens $ Lit 5 `Add` (Lit 10 `Div` Lit 2)
5 + 10 ÷ 2 = 10

λ putStrLn $ prettyPrintNoParens $ Lit 14 `Mul` (Lit 5 `Add` (Lit 10 `Div` Lit 2))
14 × 5 + 10 ÷ 2 = 140
```

Everything's looking right so far. Let's see if we can add parentheses. We can
make a very small change to our program to add parentheses naively. Each time we
print an operation using `prettyOperation` we can add parentheses to the
output. This is a small change that gets us most of the way toward the answer:

```haskell
prettyPrintSimple :: Expr -> String
prettyPrintSimple expr =
  prettyPrint' expr <> " = " <> show result
  where
    result = eval expr
    prettyPrint' e =
      case e of
        Lit n -> show n
        Sub a b -> prettyOperation " - " a b
        Add a b -> prettyOperation " + " a b
        Mul a b -> prettyOperation " × " a b
        Div a b -> prettyOperation " ÷ " a b

    prettyOperation op a b =
      "(" <> prettyPrint' a <> op <> prettyPrint' b <> ")"
```

Let's run this version and see how it compares to our earlier version with no
parentheses:

```haskell
λ putStrLn $ prettyPrintSimple $ Lit 5 `Add` Lit 10
(5 + 10) = 15

λ putStrLn $ prettyPrintSimple $ Lit 5 `Add` (Lit 10 `Div` Lit 2)
(5 + (10 ÷ 2)) = 10

λ putStrLn $ prettyPrintSimple $ Lit 14 `Mul` (Lit 5 `Add` (Lit 10 `Div` Lit 2))
(14 × (5 + (10 ÷ 2))) = 140
```

This is pretty close to our goal, but we're still generating an extra set of
parentheses. We need to print the outermost part of the expression without the
parentheses, then add them for more nested expressions. Naively, we can write
two versions of `prettyPrint'` that have our different behaviors:

```haskell
prettyPrintNoExtraParens :: Expr -> String
prettyPrintNoExtraParens expr =
  prettyPrint' expr <> " = " <> show result
  where
    result = eval expr
    prettyPrint' e =
      case e of
        Lit n -> show n
        Sub a b -> prettyOperation " - " a b
        Add a b -> prettyOperation " + " a b
        Mul a b -> prettyOperation " × " a b
        Div a b -> prettyOperation " ÷ " a b

    prettyWithParens e =
      case e of
        Lit n -> show n
        Sub a b -> "(" <> prettyOperation " - " a b <> ")"
        Add a b -> "(" <> prettyOperation " + " a b <> ")"
        Mul a b -> "(" <> prettyOperation " × " a b <> ")"
        Div a b -> "(" <> prettyOperation " ÷ " a b <> ")"

    prettyOperation op a b =
      prettyWithParens a <> op <>  prettyWithParens b
```

If we run through our examples, you'll see that this works exactly as expected:

```haskell
λ putStrLn $ prettyPrintNoExtraParens $ Lit 5 `Add` Lit 10
5 + 10 = 15

λ putStrLn $ prettyPrintNoExtraParens $ Lit 5 `Add` (Lit 10 `Div` Lit 2)
5 + (10 ÷ 2) = 10

λ putStrLn $ prettyPrintNoExtraParens $ Lit 14 `Mul` (Lit 5 `Add` (Lit 10 `Div` Lit 2))
14 × (5 + (10 ÷ 2)) = 140
```

Our code works! It's a little bit unsatisfying though. If we want to add more
operations, or change the way an operation is printed, we'd need to change it
twice. That's twice as much work even if everything goes right, and twice as
many opportunities for a mistake. Let's look at a refactored example and see how
we can do better:

```haskell
prettyPrint :: Expr -> String
prettyPrint expr =
  prettyPrint' expr <> " = " <> show result
  where
    result = eval expr
    prettyPrint' = prettyPrintWrapped id
    prettyWithParens = prettyPrintWrapped $ \pretty -> "(" <> pretty <> ")"
    prettyPrintWrapped wrapper e =
      case e of
        Lit n -> show n
        Sub a b -> wrapper $ prettyOperation " - " a b
        Add a b -> wrapper $ prettyOperation " + " a b
        Mul a b -> wrapper $ prettyOperation " × " a b
        Div a b -> wrapper $ prettyOperation " ÷ " a b
    prettyOperation op a b =
      prettyWithParens a <> op <>  prettyWithParens b
```

In this version of our code, we've factored out the decision about whether or not to
add parentheses so that it's separate from the code that displays each
expression. In our initial call from `prettyPrint'` we pass in `id`, which
leaves the rendered expression unmodified. Later, when we call the code from
`prettyWithParens` we pass in a function that will wrap the expression in
parentheses. Let's give this a shot and see if it works:

```haskell
λ putStrLn $ prettyPrint $ Lit 5 `Add` Lit 10
5 + 10 = 15

λ putStrLn $ prettyPrint $ Lit 5 `Add` (Lit 10 `Div` Lit 2)
5 + (10 ÷ 2) = 10

λ putStrLn $ prettyPrint $ Lit 14 `Mul` (Lit 5 `Add` (Lit 10 `Div` Lit 2))
14 × (5 + (10 ÷ 2)) = 140
```

Success! Like our earlier version, this function avoids adding an extra set of
parentheses to the outside of our expresion. This time, it does it without the
need to define the pretty printing function twice.

</div>
</div>
</details>
</div>
