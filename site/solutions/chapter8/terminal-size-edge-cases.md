---
chapter: 8
exercise-id: 1
name: Handling Terminal Size Edge Cases
summary: "
Summary TBD
"
---

## Handling Terminal Size Edge Cases {.problem}

In our `getTerminalSize` function there were several potential bugs that could
have occurred.  Try address these edge cases:

 - `tput` is missing
 - `tput` doesn't return a number
 - `tput` output doesn't contain a trailing newline

### Hints
<div class="hints">

<details>
<summary>Click to reveal</summary>
<div class="details-body-outer">
<div class="details-body">

</div>
</div>
</details>

<details>
<summary>Click to reveal</summary>
<div class="details-body-outer">
<div class="details-body">

</div>
</div>
</details>

<details>
<summary>Click to reveal</summary>
<div class="details-body-outer">
<div class="details-body">

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

For this exercise, let's start by narrowing our focus down to just the parts of
our program that are relevant. Specifically, the `getTerminalSize` and the
`ScreenDimensions` record. This will let us focus on the solution at hand
without too much extraneous code. As you work through the exercises for this
chapter, try to integration the solutions into your program. For now, let's look
at the version of `getTerminalSize` and `ScreenDimensions` that you should have
after finishing the chapter:

```haskell
data ScreenDimensions = ScreenDimensions
  { screenRows :: Int
  , screenColumns :: Int
  } deriving (Eq, Show)

getTerminalSize :: IO ScreenDimensions
getTerminalSize =
  case SystemInfo.os of
    "darwin" -> tputScreenDimensions
    "linux" -> tputScreenDimensions
    _other -> pure $ ScreenDimensions 25 80
  where
    tputScreenDimensions :: IO ScreenDimensions
    tputScreenDimensions =
      readProcess "tput" ["lines"] ""
      >>= \lines ->
        readProcess "tput" ["cols"] ""
        >>= \cols ->
              let lines' = read $ init lines
                  cols'  = read $ init cols
              in return $ ScreenDimensions lines' cols'
```

All of the scenarios listed in the example should throw an `IO` exception if
they fail, so focus there. Let's look at the three most compelling options for
how we might want to add error handing when an exception is thrown:

  1. Catch the exception and return a default `ScreenDimensions` value.
  2. Don't catch the exception, and let the caller deal with it.
  3. Use `Either`. Return a `Right` value on success, or `Left` for an exception.

The first of these options is pretty straightforward. Let's write a version of
`getTerminalSize` that will return a default if any exception is raised. To do
that, we'll first need to import `IOException` and `catch` from `Control.Exception`:

```haskell
import Control.Exception (IOException, catch)
```

Next, let's add a new version of `getTerminalSize` that returns a default value
when we encounter an error. We'll call our new function `getTerminalSizeWithDefault`:

```haskell
getTerminalSizeWithDefault :: IO ScreenDimensions
getTerminalSizeWithDefault =
  catch @IOException tputScreenDimensions $ \_e -> pure (ScreenDimensions 25 80)
  where
    tputScreenDimensions :: IO ScreenDimensions
    tputScreenDimensions =
      readProcess "tput" ["lines"] ""
      >>= \lines ->
        readProcess "tput" ["cols"] ""
        >>= \cols ->
              let lines' = read $ init lines
                  cols'  = read $ init cols
              in return $ ScreenDimensions lines' cols'
```



</div>
</div>
</details>

</div>
