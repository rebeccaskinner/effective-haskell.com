---
chapter: 8
exercise-id: 5
name: Scrolling Backwards
summary: "
In this exercise you'll learn how to update hcat to support scrolling backwards
as well as forwards. In the process, you'll also learn about a new kind of data
structure, called a ZipList.
"
---

## Scrolling Backwards {.problem}

Instead of just scrolling forward, update your application to allow the user to
scroll backwards as well.

### Hints
<div class="hints">

<details>
<summary>Click to reveal</summary>
<div class="details-body-outer">
<div class="details-body">

You'll need to update the definition of `ContinueCancel` to support an
additional type of user input.

<details>
<summary>See an example</summary>
```haskell
data UserInput
  = PageNext
  | PagePrevious
  | Cancel
  deriving (Eq, Show)
```
</details>

</div>
</div>
</details>

<details>
<summary>Click to reveal</summary>
<div class="details-body-outer">
<div class="details-body">

Try adding an additional parameter to `showPages` that keeps track of the pages
the user has already scrolled through.

</div>
</div>
</details>

### Solution {.solution}

<div class="solution">

<details>
<summary>Click to reveal</summary>

<div class="details-body-outer">
<div class="details-body">

Like some of the other exercises in this chapter, we can add support for
backwards scrolling by focusing on changes to only a small part of our
program. In this case, we'll need to update the way we get user input, and the
way we use that input to scroll through the contents.

Let's start with user input. In the current version of your program you probably
have something like this:

```haskell
data ContinueCancel = Continue | Cancel deriving (Eq, Show)

getContinue :: IO ContinueCancel
getContinue = do
  hSetBuffering stdin NoBuffering
  hSetEcho stdin False
  input <- getChar
  case input of
    ' ' -> pure Continue
    'q' -> pure Cancel
    _   -> getContinue
```

We'll need to make a couple of changes to support our new feature. First,
instead of simply having `Continue`, we'll need to support moving forward and
backward. A small refactoring of our type will take care of this. We'll rename
the type too, since `ContinueCancel` doesn't make much sense anymore:

```haskell
data UserInput
  = PageNext
  | PagePrevious
  | Cancel
  deriving (Eq, Show)
```

Next, we'll need to update our `continueCancel` function to support scrolling
backward. Let's rename it too:

```haskell
getInput :: IO UserInput
getInput = do
  hSetBuffering stdin NoBuffering
  hSetEcho stdin False
  input <- getChar
  case input of
    ' ' -> pure PageNext
    'b' -> pure PagePrevious
    'q' -> pure Cancel
    _   -> getInput
```

Now that we have a way to let the user try to scroll backwards, we need to
actually support backwards scrolling. The current version of your code should
look something like this:

```haskell
showPages :: [Text.Text] -> IO ()
showPages [] = return ()
showPages (page:pages) = do
  clearScreen
  TextIO.putStr page
  nextStep <- getContinue
  case nextStep of
    Continue -> showPages pages
    Cancel   -> pure ()
```

This version of our code only supports stepping forward through the pages, one
at a time. If we want to scroll backwards, we'll need to stop discarding the
pages that we've already looked at. One option would be to add an extra
parameter to `showPages` to keep track of the pages we've already looked at. The
type of our new function would be:

```haskell
showPages :: [Text] -> [Text] -> IO ()
```

If we change `showPages` then we'll also need to change all of the places where
we call it. That may be fine, but it's also not necessary. We have another
option. Since we'll always have users start at the first page, we can keep the
type of `showPages` as it was, and instead have it call a new function,
`showForwardBack` that takes both a list of previous pages and a list of next
pages.:

```haskell
showPages :: [Text] -> IO ()
showPages = showForwardBack []
```

The implementation of `showForwardBack` will be similar to our original
`showPages` function. Let's take a look at it, then we'll dig into how it works:


```haskell
showForwardBack :: [Text] -> [Text] -> IO ()
showForwardBack _ [] = return ()
showForwardBack prevPages (page:pages) = do
  clearScreen
  TextIO.putStr page
  nextStep <- getInput
  case nextStep of
    PageNext ->
      showForwardBack (page:prevPages) pages
    PagePrevious ->
      case prevPages of
        [] -> showForwardBack [] (page:pages)
        (prev:prevs) -> showForwardBack prevs (prev:page:pages)
    Cancel ->
      pure ()
```

You'll notice quite a few similarties to our old `showPages` implementation. The
biggest difference is in how we handle user input. When the user wants to move
onto the next page, we no longer discard the current page. Instead, we insert it
into a list of pages we've already seen. If the user wants to move back a page,
then the last page they viewed will be at the head of the `prevPages` list, so
we can easily pull it out of that list and stick it on the front of our list of
pages to view. We've also added a check so that a user who tries to scroll
backwards past the first page will continue to see the first page.

<br/>

<details>
<summary>Click here to see the full version of this program</summary>
```haskell
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
module EffectiveHaskell.Exercises.Chapter8.HCatBackwardsScrolling (runHCat) where

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

data FileInfo = FileInfo
  { filePath  :: FilePath
  , fileSize  :: Int
  , fileMTime :: UTCTime
  , fileReadable :: Bool
  , fileWriteable :: Bool
  , fileExecutable :: Bool
  } deriving Show

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
  } deriving (Show)


runHCat :: IO ()
runHCat = do
  hSetBuffering stdout NoBuffering
  targetFiles <- traverse getFileWithInfo =<< handleArgs
  termSize <- getTerminalSize
  showPages $ concatMap (paginate termSize) targetFiles

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

wordWrap :: Int -> Text -> [Text]
wordWrap lineLength lineText
  | Text.length lineText <= lineLength = [lineText]
  | otherwise =
    let
      (candidate, nextLines) = Text.splitAt lineLength lineText
      (firstLine, overflow) = softWrap candidate (Text.length candidate - 1)
    in firstLine : wordWrap lineLength (overflow <> nextLines)
  where
    softWrap hardwrappedText textIndex
      | textIndex <= 0 = (hardwrappedText,Text.empty)
      | Text.index hardwrappedText textIndex == ' ' =
        let (wrappedLine, rest) = Text.splitAt textIndex hardwrappedText
        in (wrappedLine, Text.tail rest)
      | otherwise = softWrap hardwrappedText (textIndex - 1)

data ScreenDimensions = ScreenDimensions
  { screenRows :: Int
  , screenColumns :: Int
  } deriving Show

paginate :: ScreenDimensions -> HCatFile -> [Text]
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

data UserInput
  = PageNext
  | PagePrevious
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
    _   -> getInput

showPages :: [Text] -> IO ()
showPages = showForwardBack []

showForwardBack :: [Text] -> [Text] -> IO ()
showForwardBack _ [] = return ()
showForwardBack prevPages (page:pages) = do
  clearScreen
  TextIO.putStr page
  nextStep <- getInput
  case nextStep of
    PageNext -> showForwardBack (page:prevPages) pages
    PagePrevious ->
      case prevPages of
        [] -> showForwardBack [] (page:pages)
        (prev:prevs) -> showForwardBack prevs (prev:page:pages)
    Cancel   -> pure ()

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
</details>

It turns out that the pattern we've implemented is actually a specific instance
of a well know data structure called a <emph>Zip List</emph> (or, sometimes, a
<emph>Zipper List</emph>).

Let's take a look at how we can use a zip list to add backward scrolling. We'll
start by creating a new module. We'll be using non-empty lists to make our
implementation a bit safer. These are part of `base`, you can read more about
them [here](https://hackage.haskell.org/package/base-4.19.0.0/docs/Data-List-NonEmpty.html).

```haskell
module EffectiveHaskell.Exercises.Chapter8.ZipList  where
import Prelude hiding (drop)
import Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List.NonEmpty as NL
```

Next, we'll define a new data type. Just like our earlier example where we did
this manually, our `ZipList` will consist of a pair of lists: elements we've
already seen, and elements we haven't yet seen. We'll also have a single focus
element:

```haskell
data ZipList a = ZipList [a] a [a]
  deriving (Eq, Show)
```

<details>
<summary>Click to read about: Empty vs. Non-Empty Zip Lists</summary>
Note that some `ZipList` implementations instead use only a pair of lists:

```haskell
data ZipList a = ZipList [a] [a]
```

This second version allows empty zip lists. We'll stick with a non-empty
version.
</details>

Next we'll need to add some functions to work with creating and traversing
through our zip lists.

The first thing we'll need to do is to support a way to create a new
`ZipList`. Let's start with the happy path and allow our users to create one
using a non-empty list:

```haskell
fromNonEmpty :: NonEmpty a -> ZipList a
fromNonEmpty (a:|as) = ZipList [] a as
```

The `(:|)` operator here how we construct (or, in this case, pattern match on) a
non-empty list. We know that we'll always have at least a single element, which
we set as the focus of our list. The rest of our list, `as` may be empty. In
either case, we set the remainder of our list to `as`. Since we're at the
beginning of our list, the list of elements we've already seen will always be
empty.

Working with non-empty lists isn't always convenient, so let's add a helper
function to construct a `ZipList` from an ordinary list. We'll need to deal with
the fact that an ordinary list might be empty, so we'll return a `Maybe` value:

```haskell
fromList :: [a] -> Maybe (ZipList a)
fromList = fmap fromNonEmpty . NL.nonEmpty
```

Next, we'll need a way to get a value from our list at the current focus
point. Let's call this function `value`. It's straightfoward, we just need to
pattern match:

```haskell
value :: ZipList a -> a
value (ZipList _ c _) = c
```

The whole point of a zip list is to make it easy to move the focus of the list
forward and backwards through the elements. Let's start by writing a `next`
function that will move the focus one element forward. If we're already at the
end of our list, we'll remain at the end rather than returning an error:

```haskell
next :: ZipList a -> ZipList a
next z@(ZipList _h _c []) = z
next (ZipList h c (x:xs)) = ZipList (c:h) x xs
```

You should recognize the similarity between this function and the way we handled
the `PageForward` case in our original `showForwardBack` function: each time we
step forward, we take the current focus and put it at the head of the list of
previous elements list, then we take the head of the next elements list and make
it the focus. If there are no more elements, then we return the zip list
unmodified.

Moving backwards through the list follows the same pattern, except we now move
the most recent previous element to the focus, and put our current focus into
the list of next items. This mirrors the `PagePrevious` case of
`showForwardBack`:

```haskell
prev :: ZipList a -> ZipList a
prev z@(ZipList [] _c _t) = z
prev (ZipList (x:xs) c t) = ZipList xs x (c:t)
```

There's only one more function we'll need to add to support our immediate
needs. In `showForwardBack` we used pattern matching to detect if we were at the
end of our list, and if so we stopped trying to show more pages and instead
exited gracefully. When working with zip lists, we don't want our users to need
to know anything about the internal implementation, so we'll provide them a
function to check to see if the focus is currently at the end of the list. We'll
call it `isEnd`:

```haskell
isEnd :: ZipList a -> Bool
isEnd (ZipList _h _c t) = null t
```
<br/>

<details>
<summary>Click here to see the complete version of this module with extra functionality</summary>
```haskell
module EffectiveHaskell.Exercises.Chapter8.ZipList  where
import Prelude hiding (drop)
import Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List.NonEmpty as NL

-- | A non-empty list with a focus on a single element.
--
-- A zip list is a non-empty list with a "focus" element. Supports
-- O(1) inserts, updates, and deletes at the current cursor.
data ZipList a = ZipList [a] a [a]
  deriving (Eq, Show)

-- | Creates a zip-list with a single element. The cursor is set to
-- the element.
singleton :: a -> ZipList a
singleton a = ZipList [] a []

-- | Construct a ZipList from a non-empty list with the focus on the
-- first element.
--
-- >>> fromNonEmpty (0 :| [1,2,3])
-- ZipList [] 0 [1,2,3]
--
fromNonEmpty :: NonEmpty a -> ZipList a
fromNonEmpty (a:|as) = ZipList [] a as

-- | Convert a ZipList to a non-empty list. The list will be in order
-- (the current location of the cursor does not change the list).
--
-- >>> toNonEmpty $ ZipList [] 0 [1,2,3]
-- 0 :| [1,2,3]
--
-- >>> toNonEmpty $ ZipList [3,2,1] 4 [5,6,7]
-- 1 :| [2,3,4,5,6,7]
--
toNonEmpty :: ZipList a -> NonEmpty a
toNonEmpty (ZipList h c t) = NL.prependList t' $ c :| t
  where t' = reverse h

-- | Attempt to create a ZipList from a list. Returns 'Nothing' if the
-- list is empty.
--
-- >>> fromList [1..5]
-- Just (ZipList [] 1 [2,3,4,5])
--
-- >>> fromList []
-- Nothing
--
fromList :: [a] -> Maybe (ZipList a)
fromList = fmap fromNonEmpty . NL.nonEmpty

-- | Convert a ZipList to a list. The current location of the cursor
-- does not change the generated list.
--
-- >>> toList $ ZipList [] 0 [1,2,3]
-- [0,1,2,3]
--
-- >>> toList $ ZipList [3,2,1] 4 [5,6,7]
-- [1,2,3,4,5,6,7]
--
toList :: ZipList a -> [a]
toList = NL.toList . toNonEmpty

-- | Insert a value "behind" the current cursor value, and move the
-- cursor to the newly inserted value.
--
-- >>> insertBack 9 (ZipList [1] 2 [3])
-- ZipList [2,1] 9 [3]
--
insertBack :: a -> ZipList a -> ZipList a
insertBack a (ZipList h c t) = ZipList (c:h) a t

-- | Insert a value "in front" the current cursor value, and move the
-- cursor to the newly inserted value.
--
-- >>> insertFront 9 (ZipList [1] 2 [3])
-- ZipList [1] 9 [2,3]
--
insertFront :: a -> ZipList a -> ZipList a
insertFront a (ZipList h c t) = ZipList h a (c:t)

-- | alias for 'insertFront'
insert :: a -> ZipList a -> ZipList a
insert = insertFront

-- | alias for 'insertBack'
append :: a -> ZipList a -> ZipList a
append = insertBack

-- | Replace the value at the cursor.
--
-- >>> update 9 $ ZipList [3,2,1] 4 [5,6,7]
-- ZipList [3,2,1] 9 [5,6,7]
update :: a -> ZipList a -> ZipList a
update a (ZipList h _ t) = ZipList h a t

-- | Remove the element at the cursor. The cursor will move toward the
-- tail of the list, except when dropping the last element in the zip
-- list.
--
-- >>> drop $ ZipList [3,2,1] 4 [5,6,7]
-- Just (ZipList [3,2,1] 5 [6,7])
--
-- >>> drop $ ZipList [3,2,1] 5 [6,7]
-- Just (ZipList [3,2,1] 6 [7])
--
-- >>> drop $ ZipList [3,2,1] 7 []
-- Just (ZipList [2,1] 3 [])
--
-- drop $ ZipList [] 1 [2,3]
-- Just (ZipList [] 2 [3])
--
-- >>> drop $ ZipList [] 1 []
-- Nothing
--
drop :: ZipList a -> Maybe (ZipList a)
drop (ZipList h _c t)
  | (x:xs) <- t = Just $ ZipList h x xs
  | (x:xs) <- h = Just $ ZipList xs x t
  | otherwise = Nothing

-- | Like 'drop' but does not check to ensure the resulting
-- list is not empty.
unsafeDrop :: ZipList a -> ZipList a
unsafeDrop (ZipList h _c t)
  | (x:xs) <- t = ZipList h x xs
  | (x:xs) <- h = ZipList xs x t
  | otherwise = error "empty ZipList"

-- | Returns 'True' if the cursor is at the start of the zip list.
isStart :: ZipList a -> Bool
isStart (ZipList h _c _t) = null h

-- | Returns 'True' if the cursor is at the end of the zip list.
isEnd :: ZipList a -> Bool
isEnd (ZipList _h _c t) = null t

-- | Set the cursor to the beginning of the list.
cursorStart :: ZipList a -> ZipList a
cursorStart z
  | isStart z = z
  | otherwise = cursorStart $ prev z

-- | Set the cursor to the end of the list.
cursorEnd :: ZipList a -> ZipList a
cursorEnd z
  | isEnd z = z
  | otherwise = cursorEnd $ next z

-- | Return the value under the cursor.
value :: ZipList a -> a
value (ZipList _ c _) = c

-- | Move the value "forward" one element (toward the tail). Has no
-- effect if the cursor is already at the end of the list.
--
-- >>> next $ ZipList [] 1 [2,3,4]
-- ZipList [1] 2 [3,4]
--
-- >>> next $ ZipList [3,2,1] 4 [5,6,7]
-- ZipList [4,3,2,1] 5 [6,7]
--
-- >>> next $ ZipList [3,2,1] 4 []
-- ZipList [3,2,1] 4 []
--
next :: ZipList a -> ZipList a
next z@(ZipList _h _c []) = z
next (ZipList h c (x:xs)) = ZipList (c:h) x xs

-- | Move the value "backwards" one element (toward the head). Has no
-- effect if the cursor is already at the start of the list.
--
-- >>> prev $ ZipList [3,2,1] 4 []
-- ZipList [2,1] 3 [4]
--
-- >>> prev $ ZipList [3,2,1] 4 [5,6,7]
-- ZipList [2,1] 3 [4,5,6,7]
--
-- >>> prev $ ZipList [] 4 [5,6,7]
-- ZipList [] 4 [5,6,7]
--
prev :: ZipList a -> ZipList a
prev z@(ZipList [] _c _t) = z
prev (ZipList (x:xs) c t) = ZipList xs x (c:t)

```
</details>

Now that we have a zip list implementation, let's return to hcat. The first
thing we'll do is change our implementation of `showForwardBack`. Instead of
taking a pair of lists, we'll take a single `ZipList` of pages. We won't pattern
match to look for an empty input, instead we'll move our check into the case
where a user asks us to move to the next page:

```haskell
showForwardBack :: ZipList Text -> IO ()
showForwardBack pages = do
  clearScreen
  TextIO.putStr $ ZipList.value pages
  nextStep <- getInput
  case nextStep of
    PageNext ->
      if ZipList.isEnd pages
      then pure ()
      else showForwardBack (ZipList.next pages)
    PagePrevious ->
      showForwardBack $ ZipList.prev pages
    Cancel -> pure ()
```

Since we've modified `showForwardBack` to require a `ZipList`, we'll also need
to update `showPages`. Let's take a look:

```haskell
showPages :: [Text] -> IO ()
showPages contents =
  whenJust showForwardBack $ ZipList.fromList contents
  where
    whenJust :: (a -> IO ()) -> Maybe a -> IO ()
    whenJust = maybe $ pure ()
```

As you can see, most of the work in our refactored function is dealing with the
fact that we might not be able to create a `ZipList` if we have an empty list of
pages. There are several ways we could write this function, try to play around
with a few other implementations.

</div>
</div>
</details>

</div>
