---
chapter: 8
exercise-id: 7
name: Terminal Resizing
summary: "
The final exercise in Chapter 8 offers you an opportunity to bring together what
you've learned in this chapter and from the previous exercises. You'll refactor
your code and think about different ways to track state in IO actions.
"
---

## Terminal Resizing {.problem}

Sometimes users will want to resize their terminals while viewing a
document. This is a problem for our pager since we calculate the size of the
terminal exactly once at the start of the program. Update your program to allow
the user to resize the terminal while retaining their current place in the
document (the first word on the screen before resizing should still be the first
word on the screen after resizing, regardless of how much the screen space has
increased or decreased).

Note that detecting changes to the size of the terminal automatically will be
unreasonably difficult with the knowledge that you have learned so far in this
book, so you should handle terminal resizing by allowing the user to press a key
to reflow the text.

### Hints
<div class="hints">

<details>
<summary>Click to reveal</summary>
<div class="details-body-outer">
<div class="details-body">

In order to handle resizing a document, you'll need to keep track of the
original contents of each document before word-wrapping. Modify your program so
that each document keeps the original version around.

</div>
</div>
</details>

<details>
<summary>Click to reveal</summary>
<div class="details-body-outer">
<div class="details-body">

Consider how to handle terminal resizing while the user is viewing help text. It
may be necessary to keep a reference to the original set of documents when
viewing help text.

</div>
</div>
</details>

<details>
<summary>Click to reveal</summary>
<div class="details-body-outer">
<div class="details-body">

Resizing a document may result in a set of pages that don't nicely align with
the position in the document before resizing. Try generating a single temporary
page with the correct position to help ease the transition between different
document sizes.

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

The final exercise in this chapter asks us to support resizing the
terminal. This presents a few challenges. The most immediately obvious problem
is that in the existing version of our program we're looking up the size of the
terminal once, when the program starts, and using that data to figure out how to
break our documents into individual pages. In order to support resizing the
terminal we'll need to deal with the fact that we might need to re-paginate our
documents at any time. We'll also need to keep track of where we are in a
document as we scroll through so that we can move back to the right location
after we resize the terminal. Finally, we'll need to re-think how we handle
displaying the help screen, and how it interacts with resizing the documents.

#### Refactor part 1: Pagination

The new version of our program introduces a lot of new requirements! Let's take
it in some smaller pieces. In the current version of our application, you should
be displaying your documents using a pair of functions: `showPages` and
`showForwardBack`. The `showPages` function takes pages of text from the help
screen, and pages of text from the documents, and converts them into zip
lists. `showPages` then passes the two lists into `showForwardBack`, which reads
input and displays individual pages until the user scrolls past the last page or
quits the program. Your code should look like something this:

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

The first obvious problem with this version of code is that we only have access
to the data we want to display after it's been formatted for a particular
terminal size. This presents two specific problems. First, each of the pages has
a footer added ahead of time. If we were to try to re-wrap the pages when the
user resizes the terminal, we'd need to make potentially fragile assumptions
about the size of the footer. Second, the word-wrapping process may need to
introduce additional newlines into the text if we have long lines. When we
resize the terminal, we shouldn't include those in the re-wrapped text. Right
now we can't tell the difference between newlines that were part of the original
document, and the ones that we introduced due to word wrapping. To address that,
we'll need to have a copy of the original document without footers or word
wrapping so that we can re-format the pages when the user resizes the terminal.

The first approach we might try to take here is to simply pass in the original
list of files along with our paginated data. For example:

```haskell
showForwardBack :: [HCatFile] -> ZipList Text -> ZipList Text -> IO ()
```

Unfortunately, this doesn't really work out. Although we have a list of the
original documents that we started with, and a `ZipList` of pages of text, we
don't actually know which document any given page of text corresponds to. We
need a way to keep track of the original unmodified version of the document
along with the pages for each document. When we reformat our document, we'll
also need to re-generate the footer, since we might have a different number of
pages. We might try to create a new `Document` record to solve these problems
like this:

```haskell
data Document = Document
  { documentContents :: Text
  , documentFooter :: Text
  , documentPages :: ZipList Text
  }
```

This is a step in the right direction, but there are still a couple of
problems. The first problem is how we're handling footers. Since the page
numbers are calculated per-page during pagination, we'll need to change this up
a bit. Instead of storing a single footer, let's store a function that
calculates a new footer given the width of the terminal, the total page count,
and current page:

```haskell
data Document = Document
  { documentContents :: Text
  , documentFooterFormatter :: Int -> Int -> Int -> Text
  , documentPages :: ZipList Text
  }
```

If we're only storing a function to calculate footers, we'd need to calculate a
footer for the current page each time we scroll to it. That's a bit
inconvenient, and it could be slow if we decided to use some heavy-weight
function. Let's create a record that will let us store the text on a page along
with the footer we've calculated for that page. We'll also update `Document` to
store pages instead of plain `Text` values:

```haskell
data Page = Page
  { pageContents :: Text
  , pageFooter :: Text
  } deriving stock (Eq, Show)

data Document = Document
  { documentContents :: Text
  , documentFooterFormatter :: Int -> Int -> Int -> Text
  , documentPages :: ZipList Text
  }
```

We'll eventually want to update our definition of `paginateWith` and to return
`Page` values instead of `Text`, but let's put that off for now since there will
be some additional changes that we'll get to soon. For now, you can add in a
placeholder for the new version of `paginateWith`:

```haskell
paginateWith :: ScreenDimensions -> (Int -> Int -> Int -> Text) -> Text -> [Page]
paginateWith (ScreenDimensions rows cols) format contents = undefined
```

We'll also need to update `showForwardBack` to handle working with documents. If
we start making changes here naively though, we could easily get ourselves into
a situation where we'll need to do some extensive refactoring. Instead, it'll be
helpful for us to take a minute to think about how our program is going to work
when we resize the terminal.

In the simplest scenario, a user will resize the terminal when they are looking
at the first page of a document that they opened. In that case, we can use the
data we've added to `Document` to simply re-paginate the document and re-display
it to the user. We might end up with a function that looks something like this:

```haskell
reflowDocument :: ScreenDimensions -> Document -> Document
reflowDocument termSize (Document contents formatFooter _) =
  let pages = unsafeZipList $ paginateWith termSize formatFooter contents
  in Document contents formatFooter pages
```

Unfortunately, while this works for our happy path, it won't meet our
requirements if the user has scrolled past the first page of the document when
the resize the terminal. We're supposed to ensure that when the user resizes the
terminal they keep their place in the document, but with this function we
recreate the list of pages from scratch when we re-paginate, causing us to lose
our place.

Reflowing the document while keeping track of our current location turns out to
be a somewhat tricky problem. The first thing we might try is to figure out what
page we're currently on, and re-scroll to that page after we re-paginate the
document. That approach won't work because a larger or smaller terminal size
means our document will be broken up into a different number of pages, so what
was the text on page 3 might now be on page 1, or page 7. The next approach we
might think about is to figure out where we are in the document by counting the
words or lines of text that we've scrolled through by looking at the current set
of pages. Unfortunately, trying to count lines or characters is unreliable since
pagination may introduce some extra newlines.

Our requirements say that when the user resizes the terminal we should ensure
that the first word on the page before resizing is still the first word on the
page after resizing. We can manage this if we track the offset in the original
document where each page starts. Let's update our `Page` record to add an offset
field:

```haskell
data Page = Page
  { pageContents :: Text
  , pageFooter :: Text
  , pageOffset :: Int
  } deriving stock (Eq, Show)
```

As we've seen, it's hard to figure out after pagination what the offset for an
individual page should be, but we can easily figure it out during the pagination
process. Let's update our `wordWrap` and `paginateWith` functions to support
finding the offsets for each page when we paginate a document. Let's start with
`wordWrap`. Our goal is to know what offset each page starts with. Pages are
groups of individual lines of text that have been word wrapped. In order to know
where a page starts, we'll need to know where each line of text begins. To do
that, we'll update `wordWrap` to take a starting offset along, along with the
line length and line text that it was already taking. Our new version of
`wordWrap` will not only return a list of wrapped lines, it'll also return the
offset that each line of text starts at.


```haskell
wordWrap :: Int -> Int -> Text -> [(Int, Text)]
wordWrap offset lineLength lineText
  | Text.length lineText <= lineLength = [(offset, lineText)]
  | otherwise =
      let
        (candidate, nextLines) = Text.splitAt lineLength lineText
        (firstLine, overflow) = softWrap candidate (Text.length candidate - 1)
        offset' = offset + Text.length firstLine
      in (offset, firstLine) : wordWrap offset' lineLength (overflow <> nextLines)
  where
    softWrap hardwrappedText textIndex
      | textIndex <= 0 = (hardwrappedText,Text.empty)
      | Text.index hardwrappedText textIndex == ' ' =
        let (wrappedLine, rest) = Text.splitAt textIndex hardwrappedText
        in (wrappedLine, Text.tail rest)
      | otherwise = softWrap hardwrappedText (textIndex - 1)
```

Now that we can keep track of where each line of wrapped text starts, we can
revisit `paginateWith`. The new version of this function will do the same job as
the previous version, but we'll need to do a bit more work to keep track of each
page's offset data. Let's take a look at the complete version and then step
through the differences in more detail:

```haskell
paginateWith :: ScreenDimensions -> (Int -> Int -> Int -> Text) -> Text -> [Page]
paginateWith (ScreenDimensions rows cols) format contents =
  let
    rows' = rows - 1
    wrappedLines = wrapWithOffset 0 $ Text.lines contents
    pageData = groupPageLines rows' wrappedLines
    pageCount = length pageData
    statusLines = map (format cols pageCount) [1..pageCount]
  in zipWith (mkPage rows') pageData statusLines
  where
    wrapWithOffset _offset [] = []
    wrapWithOffset offset (x:xs) =
      let wrapped = wordWrap offset cols x
          rest = wrapWithOffset (offset + Text.length x + 1) xs
      in wrapped <> rest

    pageDataWithOffset :: [(Int, Text)] -> (Int, [Text])
    pageDataWithOffset [] = (0, [])
    pageDataWithOffset ((minOffset, firstLine):rest) =
      (minOffset, firstLine : map snd rest)

    groupPageLines :: Int -> [(Int, Text)] -> [(Int, [Text])]
    groupPageLines pageSize = map pageDataWithOffset . groupsOf pageSize

    mkPage :: Int -> (Int, [Text]) -> Text -> Page
    mkPage pageSize (offset, pageLines) footer =
      Page
        { pageContents = Text.unlines $ padTo pageSize pageLines
        , pageFooter = footer
        , pageOffset = offset
        }

    padTo :: Int -> [Text] -> [Text]
    padTo lineCount rowsToPad =
      if length rowsToPad >= lineCount
      then rowsToPad
      else rowsToPad <> replicate (lineCount - length rowsToPad) "~"
```

The first difference in this new version of our code is the way that we're
calculating individual wrapped lines. In earlier versions of this code we took
each individual line of input and generated a list of word-wrapped lines, then
concatenated all of those lines together:

```haskell
wrappedLines = concatMap (wordWrap cols) (Text.lines contents)
```

In our new version, we're calling a new function named `wrapWithOffset`:

```haskell
wrappedLines = wrapWithOffset 0 $ Text.lines contents
```

Let's take a look in more detail at this new function:

```haskell
wrapWithOffset _offset [] = []
wrapWithOffset offset (x:xs) =
  let wrapped = wordWrap offset cols x
      rest = wrapWithOffset (offset + Text.length x + 1) xs
  in wrapped <> rest
```

Like before, we're using `wordWrap` to wrap each line of text. In this version,
we're also keeping track of the offsets for each line of wrapped text as we
go. Since our approach to word wrapping doesn't differentiate between line breaks
that were in the original document and line breaks that we've added, keeping a
running count of the offset that each line starts with will let us know where we
were in the original document.

Now that we have individual lines of text, the next thing we need to do is to
group those together into pages. In earlier versions of our program, we did it
like this:

```haskell
pages = map (Text.unlines . padTo rows') $ groupsOf rows' wrappedLines
```

Just like with word wrapping, our original approach to creating pages of text
ignored the offsets into the document where each line of text started. Now,
we'll need to use that data to figure out where each individual page
starts. We're doing that by calling `groupPageLines`:

```haskell
pageData = groupPageLines rows' wrappedLines
```

Let's take a look at the function we're calling:

```haskell
groupPageLines :: Int -> [(Int, Text)] -> [(Int, [Text])]
groupPageLines pageSize = map pageDataWithOffset . groupsOf pageSize
```

Once again we're grouping the collection of pages together into collections of
pages- but this time we're also including an offset for the entire set of
pages. We're calculating that using `pageDataWithOffset`. This function takes a
list of lines with their offset and returns all of the lines of text along with
the first offset- which should always be the minimum offset since we calculate
the pages in order:

```haskell
pageDataWithOffset :: [(Int, Text)] -> (Int, [Text])
pageDataWithOffset [] = (0, [])
pageDataWithOffset ((minOffset, firstLine):rest) =
  (minOffset, firstLine : map snd rest)
```

Once we've grouped our lines of text into pages and figured out the offset into
the document where each page starts, we need to figure out how many pages we
have in total, generate a status line function, and then return the set of
pages:

```haskell
paginateWith (ScreenDimensions rows cols) format contents =
  let
    rows' = rows - 1
    wrappedLines = wrapWithOffset 0 $ Text.lines contents
    pageData = groupPageLines rows' wrappedLines
    pageCount = length pageData
    statusLines = map (format cols pageCount) [1..pageCount]
  in zipWith (mkPage rows') pageData statusLines
```

We're using our last new helper function, `mkPage` to actually construct the
pages. This new helper is where we join the lines of text for a single page
together, add padding, and set the status line:

```haskell
    mkPage :: Int -> (Int, [Text]) -> Text -> Page
    mkPage pageSize (offset, pageLines) footer =
      Page
        { pageContents = Text.unlines $ padTo pageSize pageLines
        , pageFooter = footer
        , pageOffset = offset
        }
```

#### Documents and Pages

Now that we have a way to paginate a document while keeping track of information
like where we are in the original unwrapped document, we can re-visit our
`Document` record and think about how we can work with documents. Remember that
a `Document` is a record that has the original conents of the document, a `ZipList`
of pages that have been formatted for the current terminal size, and a footer
function to generate a status line:

```haskell
data Document = Document
  { documentContents :: Text
  , documentFooterFormatter :: Int -> Int -> Int -> Text
  , documentPages :: ZipList Page
  }
```

With this information, and our new pagination function, we can start by writing
a handful of helper functions to make it easier for us to work with entire
documents. Let's start with functions that will let us scroll forward and
backwards through the document. These functions will need to scroll through the
document's set of pages. We'll also add a couple of helpers to tell us if we're
already at the beginning or end of the document:

```haskell
documentNextPage :: Document -> Document
documentNextPage doc = doc { documentPages = ZipList.next (documentPages doc) }

documentPrevPage :: Document -> Document
documentPrevPage doc = doc { documentPages = ZipList.prev (documentPages doc) }

documentIsFirstPage :: Document -> Bool
documentIsFirstPage = ZipList.isStart . documentPages

documentIsLastPage :: Document -> Bool
documentIsLastPage = ZipList.isEnd . documentPages
```

We'll also need to be able to construct a document. We'll need the `HCatFile`
data, plus the current terminal size so we can paginate the document:

```haskell
hcatFileDocument :: ScreenDimensions -> HCatFile -> Document
hcatFileDocument termSize (HCatFile finfo contents) =
  Document
  { documentContents = contents
  , documentFooterFormatter = formatFileInfo finfo
  , documentPages = unsafeZipList $ paginateWith termSize (formatFileInfo finfo) contents
  }
```

Although our help text isn't exactly a document, it'll also make sense to treat
it as one for the sake of our program. Let's add another function that will
generate a "document” with the help text. We won't need information about a file
this time, but we'll still need screen dimensions:

```haskell
hcatHelpDocument :: ScreenDimensions -> Document
hcatHelpDocument termSize =
  Document
  { documentContents = helpText
  , documentFooterFormatter = formatHelpText
  , documentPages = unsafeZipList $ paginateWith termSize formatHelpText helpText
  }
  where
    formatHelpText cols _ _ =
      Text.take cols $ "Help Info: Press 'q' to exit" <> Text.replicate cols " "
```

The last thing we'll need to do with a `Document` is to handle resizing it when
the user resizes the terminal. In theory this should be straightforward- given a
document and a new screen size, we can just re-paginate the document:

```haskell
reflowDocument :: ScreenDimensions -> Document -> Document
reflowDocument termSize (Document contents formatter _currentPages) =
  Document
  { documentContents = contents
  , documentFooterFormatter = formatter
  , documentPages = paginateWith termSize formatter contents
  }
```

Unfortunately, this approach means that each time we re-flow the screen we'll
lose our place in the document. Remember, our goal is to ensure that when we
resize the screen we keep our current position in the document.

Unfortunately, isn't as straightforward as we might hope. We can't just scroll
forward a page at a time until we get back to where we were. After re-flowing
the document the offset at the start of the page we were viewing might now be in
the middle of a page. Instead, we'll need to return both the newly reflowed
document and a single page. This single page will show the document at the
current offset. For the `Document` we return, we'll need to scroll forward to
the first page that contains our offset.

This might sound like a lot of work, and it does end up making `reflowDocument`
fairly long, but the implementation is surprisingly straightforward thanks to
the way we've designed our application:

```haskell
reflowDocument :: ScreenDimensions -> Document -> (Page, Document)
reflowDocument termSize (Document contents formatter currentPages) =
  (reflowPage, newDocument)
  where
    currentOffset =
      pageOffset $ ZipList.value currentPages

    reflowPage =
      head
      . paginateWith termSize formatter
      . Text.drop currentOffset
      $ contents

    newPages =
      scrollToOffset currentOffset
      . unsafeZipList
      . paginateWith termSize formatter
      $ contents

    newDocument =
      Document
      { documentContents = contents
      , documentFooterFormatter = formatter
      , documentPages = newPages
      }

    scrollToOffset :: Int -> ZipList Page -> ZipList Page
    scrollToOffset offset pages
      | ZipList.isEnd pages = pages
      | nextOffset pages > offset = pages
      | otherwise = scrollToOffset offset (ZipList.next pages)

    nextOffset = pageOffset . ZipList.value . ZipList.next
```

If we're dealing with multiple documents, we'll only ever care about showing the
temporary page for the document we're actually viewing. Let's add a small helper
function for cases where we don't care about this page:

```haskell
reflowDocument' :: ScreenDimensions -> Document -> Document
reflowDocument' termSize doc = snd $ reflowDocument termSize doc
```

#### Managing The View State

Now that we can keep track of our position in each document, and reflow our
documents, we can start thinking about showing our document. At first it might
seem fairly straightforward to add support for resizing our terminal. Naively,
we can simply add a new `UserInput` that tells us to reflow our document and
continue rendering.

In theory our naive approach could work, but the interaction between resizing
our document and showing help text ends up making things a bit complicated. The
problem is that up until now we've had an implicit view state that's been
tracked in the call stack. In the current version of our program, when the user
wants to show the help screen we forget our current state and call
`showForwardBack` with the pre-formatted help text document. When that's done we
return to the current state we had before the function call and re-display the
page the user was currently on.

In our new version, no matter where a user is, we'll need to reflow all of the
open documents when they resize the screen, and even if we're showing the help
text we'll still need to make sure the reflowed document the user had been
viewing gets updated so they keep their place when they exit the help screen. In
order to make this easier, let's be more explicity about how we track
state. We'll start by adding a couple of types to help us keep track of our
state:

```haskell
data ViewMode
  = ViewContent
  | ViewHelp (ZipList Document)

data ViewState = ViewState
 { viewStateDocuments :: ZipList Document
 , viewStateCurrentMode :: ViewMode
 , viewStateCurrentTermSize :: ScreenDimensions
 }
```

Our `ViewState` record keeps track of whatever data we're currently displaying,
along with the current screen size and the view mode. `ViewMode` lets us know
whether we're viewing the original documents the user asked to view, or the help
screen. If we're viewing the help screen, we also keep track of the original set
of documents so we can restore them when we're done viewing the help text.

Now that we're explicitly tracking the state of our program, we can break a
lot of the behavior that was previously handled in `showForwardBack` out into
individual functions that modify the state:

```haskell
-- | Move the document cursor forward
--
-- Moves the cursor forward in the current document. If we're already
-- on the last page of the current document, move to the first page of
-- next document.
viewStateNextPage :: ViewState -> ViewState
viewStateNextPage (ViewState docs mode termSize)
  | documentIsLastPage (ZipList.value docs) = ViewState (ZipList.next docs) mode termSize
  | otherwise = ViewState (ZipList.modify documentNextPage docs) mode termSize

-- | Move the document cursor backwards
--
-- Move the cursor backwards in the current document. If we're on the
-- first page of the current document, move to the last page of the
-- previous document.
viewStatePreviousPage :: ViewState -> ViewState
viewStatePreviousPage (ViewState docs mode termSize)
  | documentIsFirstPage (ZipList.value docs) = ViewState (ZipList.prev docs) mode termSize
  | otherwise = ViewState (ZipList.modify documentPrevPage docs) mode termSize

-- | Check to see if we're currently viewing help text
viewStateIsHelp :: ViewState -> Bool
viewStateIsHelp viewState =
  case viewStateCurrentMode viewState of
    ViewHelp _ -> True
    _ -> False

-- | Check to see if we're on the last page of the last document
viewStateIsDone :: ViewState -> Bool
viewStateIsDone (ViewState docs _ _) =
  ZipList.isEnd docs && documentIsLastPage (ZipList.value docs)

-- | Resize everything to fit a new screen size
--
-- Resizes the documents to the new screensize. If we're currently
-- viewing help text, then also resize the original document set.
viewStateResize :: ScreenDimensions -> ViewState -> (Page, ViewState)
viewStateResize termSize (ViewState docs state _oldTermSize) =
  (newPage, ViewState newDocs newState termSize)
  where
    reflow = ZipList.map (reflowDocument' termSize)
    newPage = fst $ reflowDocument termSize (ZipList.value docs)
    newDocs = reflow docs
    newState =
      case state of
        ViewContent -> ViewContent
        ViewHelp swappedDocs -> ViewHelp (reflow swappedDocs)

-- | Start showing help text
viewStateToHelp :: ViewState -> ViewState
viewStateToHelp viewState =
  case viewStateCurrentMode viewState of
    ViewHelp _ -> viewState
    ViewContent ->
      let
        (ViewState docs _mode termSize) = viewState
        helpDocs = ZipList.singleton $ hcatHelpDocument termSize
      in ViewState helpDocs (ViewHelp docs) termSize

-- | Start showing ordinary content
viewStateToContents :: ViewState -> ViewState
viewStateToContents viewState =
  case viewStateCurrentMode viewState of
    ViewContent -> viewState
    ViewHelp docs ->
      ViewState docs ViewContent (viewStateCurrentTermSize viewState)

-- | Get the current page of the current document
viewStateCurrentPage :: ViewState -> Page
viewStateCurrentPage =
  ZipList.value . documentPages . ZipList.value . viewStateDocuments
```

These new functions are all short, and fundamentally they are just giving us an
easier way to do things we've already been doing: scrolling through our
documents, getting our current cursor location, and showing or hiding help text.

### ViewState Based Pagination

Now that we're able to manage our `ViewState` more easily, we need to do some
refactoring to the rest of our code to support using it as we're displaying
pages, scrolling, dealing with help text, and resizing our document.

Let's start by updating `showForwardBack`. So far, each time we've needed to
work with this function we've added to the complexity by adding new
features. Now that we're being more thoughtful about how we manage state, we can
take some time to simplify it. With the exception of terminal resizing, all of
the features we need are being handled by our `ViewState` helper
functions. Let's add a placeholder function for now to deal with terminal
resizing, then work on our refactored `showForwardBack`:

```haskell

handleTerminalResize :: ViewState -> IO ()
handleTerminalResize viewState = undefined

showForwardBack :: ViewState -> IO ()
showForwardBack viewState = do
  clearScreen
  printPage $ viewStateCurrentPage viewState
  nextStep <- getInput
  case nextStep of
    PageNext ->
      if viewStateIsDone viewState
      then pure ()
      else showForwardBack $ viewStateNextPage viewState
    PagePrevious ->
      showForwardBack $ viewStatePreviousPage viewState
    HelpScreen ->
      showForwardBack $ viewStateToHelp viewState
    ResizeTerminal ->
      handleTerminalResize viewState
    Cancel -> pure ()
```

This new version of `showForwardBack` has the same shape as earlier versions,
but it's now more clear that the job of this function is to map specific
keyboard inputs to functions that manipulate the view state. All that's left now
is to handle the actual terminal resizing. There's not much to do in this
function. Resizing the contents of the screen is handled by
`viewStateResize`. Our job is to check the size of the terminal with the size of
our viewstate, resize the viewstate if necessary, and display the new
document. There's one additional bit of work we'll also want to handle here:
when the user resizes the screen, we may end up with an extra page to handle the
fact that page breaks won't necessarily match. We'll need to make sure to show
the temporary page to the user as well. We'll add a second new function,
`showTempPage` to help with that:

```haskell
showTempPage :: Page -> ViewState -> IO ()
showTempPage page viewState = do
  clearScreen
  printPage page
  nextStep <- getInput
  case nextStep of
    Cancel -> pure ()
    ResizeTerminal -> handleTerminalResize viewState
    HelpScreen -> showForwardBack (viewStateToHelp viewState)
    PageNext -> showForwardBack viewState
    PagePrevious -> showForwardBack $ viewStatePreviousPage viewState

handleTerminalResize :: ViewState -> IO ()
handleTerminalResize viewState = do
  termSize <- getTerminalSize
  if termSize == viewStateCurrentTermSize viewState
    then showForwardBack viewState
    else
      let (page, viewState') = viewStateResize termSize viewState
      in showTempPage page viewState'
```

At first glance `showTempPage` resembles `showForwardBack`. Both functions do
the same general work: print the new page of text, then handle user
input. The main difference is that `showTempPage` doesn't actually scroll
forward. Instead, scrolling from the temporary page will either move the user
back, or onto the new page that contains the text at the start of the temporary
page. This will give the impression of scrolling forward or backwards by less
than a full page of text immediately after resizing.

### HCat Final Version

<details>
<summary>Expand to see the full version of the code</summary>
<div class="details-body-outer">
<div class="details-body">

```haskell
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}
module EffectiveHaskell.Exercises.Chapter8.HCatResizeTerminalPreserveState (runHCat) where

import System.Environment
import Text.Printf
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as TextIO
import qualified Data.ByteString as BS
import System.IO
import System.Info
import System.Process
import Data.Time.Clock
import Data.Time.Format
import System.Directory

import EffectiveHaskell.Exercises.Chapter8.ZipList (ZipList)
import qualified EffectiveHaskell.Exercises.Chapter8.ZipList as ZipList

data FileInfo = FileInfo
  { filePath  :: FilePath
  , fileSize  :: Int
  , fileMTime :: UTCTime
  , fileReadable :: Bool
  , fileWriteable :: Bool
  , fileExecutable :: Bool
  } deriving stock Show

clearScreen :: IO ()
clearScreen =
  BS.putStr "\^[[1J\^[[1;1H"

escapeInvertText :: Text -> Text
escapeInvertText inputStr =
  let
    reverseVideo = "\^[[7m"
    resetVideo = "\^[[0m"
  in reverseVideo <> inputStr <> resetVideo

formatFileInfo :: FileInfo -> Int -> Int -> Int -> Text
formatFileInfo FileInfo{..} maxWidth totalPages currentPage =
  let
    timestamp =
      formatTime defaultTimeLocale "%F %T" fileMTime
    permissionField :: Bool -> String -> String
    permissionField isSet s = if isSet then s else "-"
    permissionString = permissionField fileReadable "r"
                       <> permissionField fileWriteable "w"
                       <> permissionField fileExecutable "x"
    statusLine = Text.pack $
      printf "%s | permissions: %s | %d bytes | modified: %s | page: %d of %d" filePath permissionString fileSize timestamp currentPage totalPages

  in escapeInvertText (truncateStatus statusLine)
  where
    truncateStatus statusLine
      | maxWidth <= 3 = Text.replicate maxWidth "."
      | Text.length statusLine > maxWidth =
        Text.take (maxWidth - 3) statusLine <> "..."
      | otherwise = statusLine

fileInfo :: FilePath -> IO FileInfo
fileInfo filePath = do
  perms <- getPermissions filePath
  mtime <- getModificationTime filePath
  size <- BS.length <$> BS.readFile filePath
  return FileInfo
    { filePath = filePath
    , fileSize = size
    , fileMTime = mtime
    , fileReadable = readable perms
    , fileWriteable = writable perms
    , fileExecutable = executable perms
    }

data HCatSettings = HCatSettings
  { hcatLines :: Int
  , hcatFiles :: [FilePath]
  } deriving stock Show

runHCat :: IO ()
runHCat = do
  hSetBuffering stdout NoBuffering
  targetFiles <- traverse getFileWithInfo =<< handleArgs
  showPages targetFiles

handleArgs :: IO [FilePath]
handleArgs = do
  args <- getArgs
  if null args
  then ioError $ userError "no filename provided"
  else pure args

data HCatFile = HCatFile
  { hcatFileInfo :: FileInfo
  , hcatFileContents :: Text
  }

getFileWithInfo :: FilePath -> IO HCatFile
getFileWithInfo filePath = do
  contents <- TextIO.readFile filePath
  info <- fileInfo filePath
  pure HCatFile
    { hcatFileInfo = info
    , hcatFileContents = contents
    }

groupsOf :: Int -> [a] -> [[a]]
groupsOf _ [] = []
groupsOf n elems =
  let (hd, tl) = splitAt n elems
  in hd : groupsOf n tl

data ScreenDimensions = ScreenDimensions
  { screenRows :: Int
  , screenColumns :: Int
  } deriving stock (Eq, Show)

data Page = Page
  { pageContents :: Text
  , pageFooter :: Text
  , pageOffset :: Int
  } deriving stock (Eq, Show)


wordWrap :: Int -> Int -> Text -> [(Int, Text)]
wordWrap offset lineLength lineText
  | Text.length lineText <= lineLength = [(offset, lineText)]
  | otherwise =
      let
        (candidate, nextLines) = Text.splitAt lineLength lineText
        (firstLine, overflow) = softWrap candidate (Text.length candidate - 1)
        offset' = offset + Text.length firstLine
      in (offset, firstLine) : wordWrap offset' lineLength (overflow <> nextLines)
  where
    softWrap hardwrappedText textIndex
      | textIndex <= 0 = (hardwrappedText,Text.empty)
      | Text.index hardwrappedText textIndex == ' ' =
        let (wrappedLine, rest) = Text.splitAt textIndex hardwrappedText
        in (wrappedLine, Text.tail rest)
      | otherwise = softWrap hardwrappedText (textIndex - 1)

paginateWith :: ScreenDimensions -> (Int -> Int -> Int -> Text) -> Text -> [Page]
paginateWith (ScreenDimensions rows cols) format contents =
  let
    rows' = rows - 1
    wrappedLines = wrapWithOffset 0 $ Text.lines contents
    pageData = groupPageLines rows' wrappedLines
    pageCount = length pageData
    statusLines = map (format cols pageCount) [1..pageCount]
  in zipWith (mkPage rows') pageData statusLines
  where
    wrapWithOffset _offset [] = []
    wrapWithOffset offset (x:xs) =
      let wrapped = wordWrap offset cols x
          rest = wrapWithOffset (offset + Text.length x + 1) xs
      in wrapped <> rest

    pageDataWithOffset :: [(Int, Text)] -> (Int, [Text])
    pageDataWithOffset [] = (0, [])
    pageDataWithOffset ((minOffset, firstLine):rest) =
      (minOffset, firstLine : map snd rest)

    groupPageLines :: Int -> [(Int, Text)] -> [(Int, [Text])]
    groupPageLines pageSize = map pageDataWithOffset . groupsOf pageSize

    mkPage :: Int -> (Int, [Text]) -> Text -> Page
    mkPage pageSize (offset, pageLines) footer =
      Page
        { pageContents = Text.unlines $ padTo pageSize pageLines
        , pageFooter = footer
        , pageOffset = offset
        }

    padTo :: Int -> [Text] -> [Text]
    padTo lineCount rowsToPad =
      if length rowsToPad >= lineCount
      then rowsToPad
      else rowsToPad <> replicate (lineCount - length rowsToPad) "~"

helpText :: Text
helpText =
  Text.unlines
    [ "Scroll through documents one page at a time."
    , ""
    , "keybindings"
    , "<space>         Go to the next page, or exit if on the last page"
    , "b               Go back a page (unless you are on the first page)"
    , "r               Update the terminal size and reflow text"
    , "?               Show this help message"
    , ""
    , "all other keypresses will be ignored"
    ]

data UserInput
  = PageNext
  | PagePrevious
  | HelpScreen
  | ResizeTerminal
  | Cancel
  deriving stock (Eq, Show)

getInput :: IO UserInput
getInput = do
  hSetBuffering stdin NoBuffering
  hSetEcho stdin False
  input <- getChar
  case input of
    ' ' -> pure PageNext
    'b' -> pure PagePrevious
    'q' -> pure Cancel
    'r' -> pure ResizeTerminal
    '?' -> pure HelpScreen
    _   -> getInput

unsafeZipList :: [a] -> ZipList a
unsafeZipList as =
  case ZipList.fromList as of
    Just as' -> as'
    Nothing -> error "expected a non-empty zip list"

data ViewMode
  = ViewContent
  | ViewHelp (ZipList Document)

data Document = Document
  { documentContents :: Text
  , documentFooterFormatter :: Int -> Int -> Int -> Text
  , documentPages :: ZipList Page
  }

documentNextPage :: Document -> Document
documentNextPage doc = doc { documentPages = ZipList.next (documentPages doc) }

documentPrevPage :: Document -> Document
documentPrevPage doc = doc { documentPages = ZipList.prev (documentPages doc) }

documentIsFirstPage :: Document -> Bool
documentIsFirstPage = ZipList.isStart . documentPages

documentIsLastPage :: Document -> Bool
documentIsLastPage = ZipList.isEnd . documentPages

hcatFileDocument :: ScreenDimensions -> HCatFile -> Document
hcatFileDocument termSize (HCatFile finfo contents) =
  Document
  { documentContents = contents
  , documentFooterFormatter = formatFileInfo finfo
  , documentPages = unsafeZipList $ paginateWith termSize (formatFileInfo finfo) contents
  }

hcatHelpDocument :: ScreenDimensions -> Document
hcatHelpDocument termSize =
  Document
  { documentContents = helpText
  , documentFooterFormatter = formatHelpText
  , documentPages = unsafeZipList $ paginateWith termSize formatHelpText helpText
  }
  where
    formatHelpText cols _ _ =
      Text.take cols $ "Help Info: Press 'q' to exit" <> Text.replicate cols " "

reflowDocument :: ScreenDimensions -> Document -> (Page, Document)
reflowDocument termSize (Document contents formatter currentPages) =
  (reflowPage, newDocument)
  where
    currentOffset =
      pageOffset $ ZipList.value currentPages

    reflowPage =
      head
      . paginateWith termSize formatter
      . Text.drop currentOffset
      $ contents

    newPages =
      scrollToOffset currentOffset
      . unsafeZipList
      . paginateWith termSize formatter
      $ contents

    newDocument =
      Document
      { documentContents = contents
      , documentFooterFormatter = formatter
      , documentPages = newPages
      }

    scrollToOffset :: Int -> ZipList Page -> ZipList Page
    scrollToOffset offset pages
      | ZipList.isEnd pages = pages
      | nextOffset pages > offset = pages
      | otherwise = scrollToOffset offset (ZipList.next pages)

    nextOffset = pageOffset . ZipList.value . ZipList.next

reflowDocument' :: ScreenDimensions -> Document -> Document
reflowDocument' termSize doc = snd $ reflowDocument termSize doc

data ViewState = ViewState
  { viewStateDocuments :: ZipList Document
  , viewStateCurrentMode :: ViewMode
  , viewStateCurrentTermSize :: ScreenDimensions
  }

-- | Move the document cursor forward
--
-- Moves the cursor forward in the current document. If we're already
-- on the last page of the current document, move to the first page of
-- next document.
viewStateNextPage :: ViewState -> ViewState
viewStateNextPage (ViewState docs mode termSize)
  | documentIsLastPage (ZipList.value docs) = ViewState (ZipList.next docs) mode termSize
  | otherwise = ViewState (ZipList.modify documentNextPage docs) mode termSize

-- | Move the document cursor backwards
--
-- Move the cursor backwards in the current document. If we're on the
-- first page of the current document, move to the last page of the
-- previous document.
viewStatePreviousPage :: ViewState -> ViewState
viewStatePreviousPage (ViewState docs mode termSize)
  | documentIsFirstPage (ZipList.value docs) = ViewState (ZipList.prev docs) mode termSize
  | otherwise = ViewState (ZipList.modify documentPrevPage docs) mode termSize

-- | Check to see if we're currently viewing help text
viewStateIsHelp :: ViewState -> Bool
viewStateIsHelp viewState =
  case viewStateCurrentMode viewState of
    ViewHelp _ -> True
    _ -> False

-- | Check to see if we're on the last page of the last document
viewStateIsDone :: ViewState -> Bool
viewStateIsDone (ViewState docs _ _) =
  ZipList.isEnd docs && documentIsLastPage (ZipList.value docs)

-- | Resize everything to fit a new screen size
--
-- Resizes the documents to the new screensize. If we're currently
-- viewing help text, then also resize the original document set.
viewStateResize :: ScreenDimensions -> ViewState -> (Page, ViewState)
viewStateResize termSize (ViewState docs state _oldTermSize) =
  (newPage, ViewState newDocs newState termSize)
  where
    reflow = ZipList.map (reflowDocument' termSize)
    newPage = fst $ reflowDocument termSize (ZipList.value docs)
    newDocs = reflow docs
    newState =
      case state of
        ViewContent -> ViewContent
        ViewHelp swappedDocs -> ViewHelp (reflow swappedDocs)

-- | Start showing help text
viewStateToHelp :: ViewState -> ViewState
viewStateToHelp viewState =
  case viewStateCurrentMode viewState of
    ViewHelp _ -> viewState
    ViewContent ->
      let
        (ViewState docs _mode termSize) = viewState
        helpDocs = ZipList.singleton $ hcatHelpDocument termSize
      in ViewState helpDocs (ViewHelp docs) termSize

-- | Start showing ordinary content
viewStateToContents :: ViewState -> ViewState
viewStateToContents viewState =
  case viewStateCurrentMode viewState of
    ViewContent -> viewState
    ViewHelp docs ->
      ViewState docs ViewContent (viewStateCurrentTermSize viewState)

-- | Get the current page of the current document
viewStateCurrentPage :: ViewState -> Page
viewStateCurrentPage =
  ZipList.value . documentPages . ZipList.value . viewStateDocuments

showPages :: [HCatFile] -> IO ()
showPages inputFiles = do
  termSize <- getTerminalSize
  showForwardBack $ ViewState
    { viewStateDocuments = unsafeZipList $ map (hcatFileDocument termSize) inputFiles
    , viewStateCurrentMode = ViewContent
    , viewStateCurrentTermSize = termSize
    }

printPage :: Page -> IO ()
printPage page =
  TextIO.putStr (pageContents page <> pageFooter page)

showTempPage :: Page -> ViewState -> IO ()
showTempPage page viewState = do
  clearScreen
  printPage page
  nextStep <- getInput
  case nextStep of
    Cancel -> pure ()
    ResizeTerminal -> handleTerminalResize viewState
    HelpScreen -> showForwardBack (viewStateToHelp viewState)
    PageNext -> showForwardBack viewState
    PagePrevious -> showForwardBack $ viewStatePreviousPage viewState

handleTerminalResize :: ViewState -> IO ()
handleTerminalResize viewState = do
  termSize <- getTerminalSize
  if termSize == viewStateCurrentTermSize viewState
    then showForwardBack viewState
    else
      let (page, viewState') = viewStateResize termSize viewState
      in showTempPage page viewState'

showForwardBack :: ViewState -> IO ()
showForwardBack viewState = do
  clearScreen
  printPage $ viewStateCurrentPage viewState
  nextStep <- getInput
  case nextStep of
    PageNext ->
      if viewStateIsDone viewState
      then pure ()
      else showForwardBack $ viewStateNextPage viewState
    PagePrevious ->
      showForwardBack $ viewStatePreviousPage viewState
    HelpScreen ->
      showForwardBack $ viewStateToHelp viewState
    ResizeTerminal ->
      handleTerminalResize viewState
    Cancel -> pure ()

getTerminalSize :: IO ScreenDimensions
getTerminalSize =
  case System.Info.os of
    "darwin" -> tputScreenDimensions
    "linux" -> tputScreenDimensions
    _otherwise ->
      ioError . userError
      $ printf "Unsupported platform: %s" System.Info.os
  where
    tputScreenDimensions :: IO ScreenDimensions
    tputScreenDimensions = do
      termLines <- readProcess "tput" ["lines"] ""
      termCols  <- readProcess "tput" ["cols"] ""
      return ScreenDimensions
          { screenRows = read . init $ termLines
          , screenColumns = read . init $ termCols
          }
```

</div>
</div>
</details>



</div>
</div>
</details>


</div>
