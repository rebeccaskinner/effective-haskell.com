---
chapter: 8
exercise-id: 6
name: Adding A Help Screen
summary: "
This exercise will help you learn how to think about nested IO actions while you
expand your program to support a help screen.
"
---

## Adding A Help Screen {.problem}

Allow the user to view help text on how to use the program by entering `?` while
viewing a file. The program should clear the screen and display a help
message. Once the user scrolls past the end of the help text, or presses `q` the
program should return them to where they were in the document.

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

Unlike the last few examples for Chapter 8, this exercise presents us with a
little bit of a conundrum. We need to support showing a new type of content, a
help screen, and this new content doesn't fit neatly in with the design of the
rest of our application.

The first problem we run into is that we've built our program around the idea
that we'll always be paginating files. The idea of a file metadata is baked into
how we generate page data. Our help text will be hard coded, so we'll need to
either have a second implementation of our pagination code for hard-coded help
text, refactor our pagination code to deal with different kinds of status bars,
or create some fake file metadata.

Creating fake file metadata would perhaps be the most expedient approach, and
there's an argument for it as an easy incremental change, but it would require
that we provide some meaningless data that could potentially confuse our
users. Having two different pagination functions, one specific to help text,
would harm the long term maintainability of our program. That leaves refactoring
our pagination code as our next best option.

Right now you probably have a version of `paginate` that looks something like
this:

```haskell
paginate :: ScreenDimensions -> HCatFile -> [Text.Text]
paginate (ScreenDimensions rows cols) (HCatFile finfo contents) =
  let
    rows' = rows - 1
    wrappedLines = concatMap (wordWrap cols) (Text.lines contents)
    pages = map (Text.unlines . padTo rows') $ groupsOf rows' wrappedLines
    pageCount = length pages
    statusLines = map (formatFileInfo finfo cols pageCount) [1..pageCount]
  in zipWith (<>) pages statusLines
  where
    padTo :: Int -> [Text] -> [Text]
    padTo lineCount rowsToPad =
      if length rowsToPad >= lineCount
      then rowsToPad
      else rowsToPad <> replicate (lineCount - length rowsToPad) "~"
```

The important thing to note about this version of our function is that our
status line is being created using the hard-coded call to `formatFileInfo`. If
we replace this with a call to a user-supplied function, then we can let the
user supply any sort of status bar they want. That also means that we don't need
to take an `HCatFile`, since we were only using the file metadata in it to call
`formatFileInfo`. Let's call our newly refactored function `paginateWith` since
we'll do pagination <emph>with</emph> some user supplied function. Let's take
a look at the implementation and then dive into the details:

```haskell
paginateWith :: ScreenDimensions -> (Int -> Int -> Int -> Text) -> Text -> [Text]
paginateWith (ScreenDimensions rows cols) formatStatusBar contents =
  let
    rows' = rows - 1
    wrappedLines = concatMap (wordWrap cols) (Text.lines contents)
    pages = map (Text.unlines . padTo rows') $ groupsOf rows' wrappedLines
    pageCount = length pages
    statusLines = map (formatStatusBar cols pageCount) [1..pageCount]
  in zipWith (<>) pages statusLines
  where
    padTo :: Int -> [Text] -> [Text]
    padTo lineCount rowsToPad =
      if length rowsToPad >= lineCount
      then rowsToPad
      else rowsToPad <> replicate (lineCount - length rowsToPad) "~"
```

You'll notice that most of the body of our function is unchanged. In fact,
within the body of the function the only change is that we changed the
definition of `statusLines`. Here are the two versions side-by-side:

```haskell
-- before
statusLines = map (formatFileInfo finfo cols pageCount) [1..pageCount]

-- after
statusLines = map (formatStatusBar cols pageCount) [1..pageCount]
```

As you can see, the only change is that we've replaced a call to
`formatFileInfo` with a call to `formatStatusBar`. This new function is an
argument passed in by our user. The function takes a terminal width, a total
page count, and a current page, and returns a formatted status bar. It has the
type `(Int -> Int -> Int -> Text)`. This isn't easiest type signature to read,
and in a production application we might think about using `newtype` wrappers or
type aliases to make it more obvious what sort of value to use for each `Int`.
now, we'll avoid the digression and stick with `Int`.

Our new `paginateWith` function buys us flexibility, and removes the dependency
on having file metadata, but our original pagination function was working
perfectly well for all of our needs unrelated to showing help text. Thankfully,
we can write it in terms of our new `paginateWith` function. This means we don't
need to refactor any existing calls to `paginate`, we just need to pass our
original call to `formatFileInfo` in as an argument.

```haskell
paginate :: ScreenDimensions -> HCatFile -> [Text]
paginate terminalSize (HCatFile finfo contents) =
  paginateWith terminalSize (formatFileInfo finfo) contents
```

Now that we've re-created our original `paginate` function for ordinary file
contents, let's write a second function to let us paginate our help text. We'll
call this one `paginateHelpText`. For this function, we don't really care about
showing the current page number or total page count, we just want to dispaly a
short message letting the user know they are viewing help text:

```haskell
paginateHelpText :: ScreenDimensions -> Text -> [Text]
paginateHelpText terminalSize =
  paginateWith terminalSize helpStatusBar
  where
    helpStatusBar cols _ _ =
      Text.take cols $ "Help Info: Press 'q' to exit" <> Text.replicate cols " "
```

Thanks to our newly refactored `paginateWith` function, we're able to reuse all
of our word wrapping and pagination code while still providing a custom status
bar implementation for our help text.

Now that we have a way to format help text, let's go ahead and actually write
some help text:

```haskell
helpText :: Text
helpText =
  Text.unlines
    [ "Scroll through documents one page at a time."
    , ""
    , "keybindings"
    , "<space>         Go to the next page, or exit if on the last page"
    , "b               Go back a page (unless you are on the first page)"
    , "?               Show this help message"
    , ""
    , "all other keypresses will be ignored"
    ]
```

We have help text, and a way to format it. This brings us to our next
problem. How should we go about putting those things together? Right now we
always calculate the size of our terminal once, at the start of our
program, and use it to paginate the contents of our file. We'll need the
terminal size to paginate the help text as well. Let's update `runHCat`to create
paginated help text along with our paginated file contents:

```haskell
runHCat :: IO ()
runHCat = do
  hSetBuffering stdout NoBuffering
  targetFiles <- traverse getFileWithInfo =<< handleArgs
  termSize <- getTerminalSize
  let helpPages = paginateHelpText termSize helpText
  showPages helpPages $ concatMap (paginate termSize) targetFiles
```

You'll notice that we're not only creating paginated help information, we're
also passing it along as a new argument to `showPages`. This is the second issue
with our current approach. Since we always calculate the terminal size at the
start of our program, we end up needing to pass all of our paginated text
around. As long as we're only dealing with two different kinds of text it's not
the end of the world, but as you'll see in the next exercise we do get some
benefits from pushing terminal size calculation out to the edge of our program.

For now, let's stick with our current approach and move on to `showPages`. We
know that we'll need to add an extra argument with our help text. If you've been
following along with the exercises so far in order, you probably have a version
of `showPages` that looks something like this:

```haskell
showPages :: [Text] -> IO ()
showPages contents =
  whenJust showForwardBack $ ZipList.fromList contents
  where
    whenJust :: (a -> IO ()) -> Maybe a -> IO ()
    whenJust = maybe $ pure ()
```

This version of `showPages` takes a list of pages containing the contents of our
files, converts the list into a zip list, and then passes it along to
`showForwardBack`. Our new version will do the same thing, but we'll convert
both our paginated file contents and our paginated help text into two separate
zip lists, and pass both of them to `showForwardBack`:

```haskell
showPages :: [Text] -> [Text] -> IO ()
showPages help contents =
  case result of
    Nothing -> pure ()
    Just runResult -> runResult
  where
    result :: Maybe (IO ())
    result = do
      contents' <- ZipList.fromList contents
      help' <- ZipList.fromList help
      pure $ showForwardBack help' contents'
```

Before we move on to `showForwardBack` let's take a quick detour to `UserInput`
and `getInput`. In the last exercise we updated these to support moving
backwards to a previous page. Following the same pattern, let's update them
again to add support for showing help text:

```haskell
data UserInput
  = PageNext
  | PagePrevious
  | HelpScreen
  | Cancel
  deriving (Eq, Show)

getInput :: IO UserInput
getInput = do
  hSetBuffering stdin NoBuffering
  hSetEcho stdin False
  input <- getChar
  case input of
    ' ' -> pure PageNext
    'b' -> pure PagePrevious
    'q' -> pure Cancel
    '?' -> pure HelpScreen
    _   -> getInput
```

Finally, let's move on to `showForwardBack`. We know from our changes to
`showPages` that we'll be adding a new argument to hold our help text, and our
changes to `UserInput` mean that we'll need to add a new case to our `case`
expression to deal with `HelpScreen` input. Knowing these two things can help
drive us most of the way toward the changes we still need to make. If we
momentarily ignore the goal of showing help text, and just aim to get our
program to compile, we might end up with something like this:

```haskell
showForwardBack :: ZipList Text -> ZipList Text -> IO ()
showForwardBack help pages = do
  clearScreen
  TextIO.putStr $ ZipList.value pages
  nextStep <- getInput
  case nextStep of
    PageNext ->
      if ZipList.isEnd pages
      then pure ()
      else showForwardBack help (ZipList.next pages)
    PagePrevious ->
      showForwardBack help $ ZipList.prev pages
    HelpScreen -> pure ()
    Cancel -> pure ()
```

From here, we can start to reason about how we want to show the help text. There
are a couple of things we know:

  1. Showing the help text shouldn't move us backwards or forewards in the
     list of pages from the files we're viewing
  2. Viewing the help text should work just like viewing the contents of a file,
     and our help text is formatted the same way that file contents are
     formatted

Putting these two pieces of information together, we can realize that when the
user asks us to show the help text, we can do that by first calling
`showForwardBack` and replacing `pages` with the help text. This will let the
user scroll through the text or do anything else they'd like. When they are
finished scrolling through the help, `showForwardBack` will return, and from
there we can continue where we left off in our list of file content pages. Let's
take a look:

```haskell
showForwardBack :: ZipList Text -> ZipList Text -> IO ()
showForwardBack help pages = do
  clearScreen
  TextIO.putStr $ ZipList.value pages
  nextStep <- getInput
  case nextStep of
    PageNext ->
      if ZipList.isEnd pages
      then pure ()
      else showForwardBack help (ZipList.next pages)
    PagePrevious ->
      showForwardBack help $ ZipList.prev pages
    HelpScreen -> do
      showForwardBack help help
      showForwardBack help pages
    Cancel -> pure ()
```


</div>
</div>
</details>

</div>
