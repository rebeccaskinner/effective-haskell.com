---
chapter: 8
exercise-id: 1
name: Handling Terminal Size Edge Cases
summary: "
Errors happen when you are dealing with IO, and it's important to handle them
gracefully. In this exercise you'll have the opportunity to look at a couple of
common ways to deal with exceptions in a real application, and evaluate
the tradeoffs of each approach.
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

You'll notice in this that our code has gotten a bit shorter, but still looks
largely similar to the earlier version. We've kept the original definition of
`tputScreenDimensions`, but now we're calling it through `catch` and returning
a default `ScreenDimensions` if there are any exceptions. Since we'll be
handling errors with `catch` we no longer need to check the operating system as
a way of guessing whether or not `tput` is likely to be installed.

The next approach we identified was to avoid catching any exceptions in
`getTerminalSize` and, instead, to catch an exception at the call site and deal
with it there. Right now we're calling `getTerminalSize` from `runHCat`:

```haskell
runHCat :: IO ()
runHCat =
  handleArgs
  >>= eitherToErr
  >>= flip openFile ReadMode
  >>= TextIO.hGetContents
  >>= \contents ->
    getTerminalSize >>= \termSize ->
      let pages = paginate termSize contents
      in showPages pages
```

Let's take a look at how we could handle an error in this function
instead. We'll still default to a 25x80 terminal if we can't get a default
terminal size, but this time we'll show the user a message telling them what
failed and letting them know that we're falling back to a default terminal size.

We'll do this by adding a new `where` binding named `terminalSizeWithErr`:

```haskell
terminalSizeWithErr = catch @IOError getTerminalSize $ \err ->
  Clock.getCurrentTime >>= \now ->
    let defaultTermSize = ScreenDimensions 25 80
        finfo = FileInfo "" 0 now False False False
        errText = Text.pack $
          "An error occurred while trying to get the screen dimensions:\n"
          <> show err
          <> "\nDefaulting to a terminal size of 80x25"
        msg = paginate defaultTermSize finfo errText
    in showPages msg >> pure defaultTermSize
```

As you can see, our new error handling function is quite a bit bigger than the
error handling we added when we defined `getTerminaSizeWithDefault`, but we're
also getting a much more featureful error handling implementation. We can
temporarily change the call to `tput` to something that doesn't exist so that we
can see our error handling working. You should see a message like this:

```
An error occurred while trying to get the screen dimensions:
tput-bad: readCreateProcess: posix_spawnp: does not exist (No such file or
directory)
Defaulting to a terminal size of 80x25
```

Notice that in this example output the text is wrapped to 80 characters. You can
also see in this screenshot that the text is wrapped to 80 characters even
though the terminal is larger:

![A screenshot of hcat error output wrapped to 80 columns](/images/solutions/chapter8/tput-error.webp)


</div>
</div>
</details>

</div>
