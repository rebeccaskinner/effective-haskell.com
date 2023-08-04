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

This exercise asks us to consider three different potential errors. For the
moment we'll focus on the first problem: what should we do if `tput` is
missing. The next solution on this page will cover the remaining errors.

For now, let's narrow our focus down to just the parts of our our program that
are relevant. Specifically, the `getTerminalSize` and the `ScreenDimensions`
record. This will let us focus on the solution at hand without too much
extraneous code. As you work through the exercises for this chapter, try to
integration the solutions into your program. For now, let's look at the version
of `getTerminalSize` and `ScreenDimensions` that you should have after finishing
the chapter:

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

If `tput` is missing, it should throw an `IO` exception, so lets focus on
that. There are a couple of different ways we can go about dealing with an
exception caused by a missing executable:

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
runHCat = do
  targetFilePath <- do
    args <- handleArgs
    eitherToErr args
  contents <- do
    handle <- openFile targetFilePath ReadMode
    TextIO.hGetContents handle
  termSize <- getTerminalSize
  hSetBuffering stdout NoBuffering
  finfo <- fileInfo targetFilePath
  let pages = paginate termSize finfo contents
  showPages pages
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

Finally, let's look at how we might use `Either` for error handling. We can
start by making a minor change to `terminalSizeWithErr` to return a `Left` value
instead of a default `ScreenDimensions` when we catch an exception:

```haskell
getTerminalSizeEither =
  catch @IOException (Right <$> tputScreenDimensions) $ \e -> pure $ Left (show e)
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

Since we're returning an explicit error, this function isn't a drop-in
replacement for `getTerminalSize` or `terminalSizeWithErr`. We'll need to handle
the error and then either exit or return a default value. We'll need to add a
few functions to do this, so let's go ahead and take a look at all of them
together:


```haskell
  where
    defaultScreenDimensions = ScreenDimensions 25 80
    showError finfo termSize err =
      showPages $ paginate termSize finfo err
    termSizeWithDefault finfo defaultTermSize = do
      termSize <- getTerminalSizeEither
      case termSize of
        Left err -> do
          showError finfo defaultTermSize (Text.pack err)
          pure defaultTermSize
        Right termSize' -> pure termSize'
     getTerminalSizeEither =
      catch @IOException (Right <$> tputScreenDimensions) $ \e -> pure $ Left (show e)
    tputScreenDimensions =
      readProcess "tput" ["lines"] ""
      >>= \lines ->
        readProcess "tput" ["cols"] ""
        >>= \cols ->
              let lines' = read $ init lines
                  cols'  = read $ init cols
              in return $ ScreenDimensions lines' cols'
```

In this example we've left `getTerminalSizeEither` and `tputScreenDimensions`
unchanged, but we've added three new `where` bindings. Like our other examples,
we need to create a default terminal size to use if we've encountered an
error. In this example, it's called `defaultScreenDimensions`. We've also added
a new function, `showError` that will print an error message to the
screen. Unlike the last example, we're not creating a rich error message
here. You're welcome to add a more robust error in your example if you
prefer. The last function we've added is `termSizeWithDefault`. This function
tries to get a terminal size, checks to see if we've gotten an error, and if so
prints the error message before returning a default value.

We've now looked at three different approaches to handling a missing `tput`
executable. All of the approaches we've tried have been pretty similar in how
they've dealt with errors. Where they differ is in where the errors are handled
and how they are communicated back to our user. There will be times when each of
the different approaches we've taken will be useful, so let's lay out the pros
and cons explicitly.

First, we wrote a function that handled the missing `tput` error entirely
internally and returned a default value. Handling the error when it happens
simplifies things for our callers, since they neither need to know about this
failure case nor explicitly handle it. The biggest drawback to this style of
error handling is that it's inflexible. In `getTerminalSizeWithDefault` we're
not giving the caller any choice about what to do when an error occurs. If the
caller wants to print an error message, try a different method of getting the
terminal size, or even exit the application they are out of luck. When something
goes wrong, caller won't even know that an error ocurred. The tradeoffs here
mean that this style of error handling is best used for private functions that are
internal to a module and not exported. When the function isn't general purpose
and we know that it's handling errors the way we want them handled, then it's a
more worthwhile tradeoff to keep the interface to the function easy to use.

Next we wrote a version of our function that didn't do any error handling, and
we allowed the caller to deal with the IO exception themselves. This approach
gives our caller all of the power. They can catch the exception and handle it
however they want- or they can ignore it entirely. If the caller chooses to
ignore the error it will bubble upwards until it's either caught by something
further up the callstack, or our program exits. It's always possible for IO
actions to raise exceptions, so we might expect that the caller will be
expecting exceptions and handling them already. In reality though, assuming that
users will know to deal with IO exceptions works best when the exceptions are
truly exceptional. We're assuming `tput` will exist, and it's a pretty common
utility so our example might pass that test, but it's something that we should
think about. When we don't catch the exception, we're telling our users that
this failure case should be treated the same as other exceptional situations
that might happen we we're doing IO.

Finally, we wrote a version of our code that uses `Either` rather than
exceptions for handling a bad call to `tput`. Of all the options we've looked
at, this is the best default approach to handling errors. Just like in our last
example, we're still letting the caller decide how they want to handle
errors. Since we're explicit about the fact that we might return an error if
`tput` isn't avaialble, we don't need to worry about the caller not realizing
failure is a possibility, so our code is much more likely to be used safely.

</div>
</div>
</details>

<details>
<summary>Click to reveal</summary>
<div class="details-body-outer">
<div class="details-body">

The last part of this solution spent a lot of time looking at different
approaches to handling IO exceptions when a call to `tput` fails, but we still
have to other edge cases to consider:

 - `tput` doesn't return a number
 - `tput` output doesn't contain a trailing newline

In the last part of this exersise we focused a lot of our time on catching
IO exceptions- either inside of the function that gets the terminal size, or
outside of it. Unfortunately neither of these errors will generate an IO
exception. We can easily write a quick test to validate that:

```haskell
module EffectiveHaskell.Exercises.Chapter8.ReadError where
import Control.Exception

readWithCatch :: String -> IO Int
readWithCatch input =
  catch @IOException readInput $ \_e -> pure 0
  where
    readInput = pure . read $ input
```

If we call `readWithCatch` and give it something other than a number, we'll
get a runtime error instead of `0`:

```haskell
λ readWithCatch "0"
0

λ readWithCatch "zero"
*** Exception: Prelude.read: no parse
```

It turns out that this situation isn't entirely hopeless. The `evaluate`
function from `Control.Exception` will let us create a new `IO` action from a
pure value, and in the process any runtime exceptions will get thrown and we can
`catch` them. Let's try it out:

```haskell
readWithCatch' :: String -> IO Int
readWithCatch' input =
  catch @ErrorCall readInput $ \_e -> pure 0
  where
    readInput = evaluate $ read input
```

You'll notice in this example we've changed the type of the exception we're
catching. Not all exceptions are `IOException`s, even though we're dealing with
an `IO`
action. [`ErrorCall`](https://hackage.haskell.org/package/base-4.18.0.0/docs/Control-Exception.html#t:ErrorCall)
is raised when something calls the `error` function.

This version of our code works, and we can test it out in `ghci`:

```haskell
λ readWithCatch' "zero"
0
λ readWithCatch' "1"
1
λ readWithCatch' "2"
2
λ readWithCatch' "three"
0
```

Clearly this code works as expected, but is it a good design? Using `evaluate`
means that we're turning otherwise pure code into an `IO` action so that we can
handle errors as exceptions. There are situations where this is a reasonable
design decision- for example writing tests where we want to catch and assert on
errors, or writing a server where an `error` while handling a particular request
should not take down the entire server. Still, when possible we should aim to
keep pure code pure. Thankfully, we have another option. The `Text.Read` module
in `base` has a function named `readEither` that will return an actual error
value instead of calling `error`:

```haskell
λ import Text.Read

λ readEither @Int "1"
Right 1

λ readEither @Int "2"
Right 2

λ readEither @Int "three"
Left "Prelude.read: no parse"
```

Getting back a pure `Either` value seems like a much nicer approach than
`evaluate`, so let's stick with it for now. There are two error cases that we
need to handle:

 - `tput` doesn't return a number
 - `tput` output doesn't contain a trailing newline

`readEither` solves the first problem, but we still need to address the second
problem. Our existing code assumes that the output we get will always be
newline-terminated, so it uses `init` to drop the last character. If the output
doesn't end with a newline for some reason, then we'll remove a character that
should have been part of the value we want to parse. Let's write a function to
handle this case.

There are two ways we might handle this. The "flexible” approach would say that
we should drop a newline at the end of the string if one is present, and
otherwise just try to parse the string as-is. The "strict” approach would
instead return an error if the string doesn't end in a newline as we
expect. For our purposes, we expect that the output should always be newline
terminated. If there's no newline, we can't be sure the rest of the text is
reliable, so we'll go with the strict approach.

Let's write a function called `nonEmptyStrStripNewline` that will take a
non-empty string and

</div>
</div>
</details>

</div>
