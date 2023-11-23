---
chapter: 8
exercise-id: 4
name: Viewing Multiple Files
summary: "
Learn how to refactor your program to deal with multiple files. This exercise
will give you experience working with refactoring and will help you build a more
useful program.
"
---

## Viewing Multiple Files {.problem}

Expand your application to allow the user to pass more than one file in on the
command line, and view them in order. Make sure to update the status line when
you go from showing one file to another.


### Hints
<div class="hints">

<details>
<summary>Click to reveal</summary>
<div class="details-body-outer">
<div class="details-body">

Each file that you work with will need to have it's own metadata. Look for a way
to do pagination once per file.

</div>
</div>
</details>

<details>
<summary>Click to reveal</summary>
<div class="details-body-outer">
<div class="details-body">

You should remove the part of your program that checks to ensure only a single
argument was passed in on the command line. Instead, try to make your program
work with 0 or more arguments. If the user doesn't pass in any arguments, you
can exit without printing anything to the screen.

</div>
</div>
</details>

<details>
<summary>Click to reveal</summary>
<div class="details-body-outer">
<div class="details-body">

The `traverse` will let you run `IO` actions once for each element of a
list. For example, imagine you want read the contents from a list of files:

```haskell
λ traverse readFile ["/tmp/a", "/tmp/b"]
["file contents a","file contents b"]
```

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

Extending our program to support multiple files offers us a lot of opportunities
to learn where the design of our program is flexible, and where it's not so
flexible. There are several different approaches we can take to refactoring our
program to support multiple inputs, and after reviewing the solution presented
here you may still want to try some other approaches.

For now, we'll focus on how we can support multiple files with a relatively
small and non-intrusive set of changes. Thankfully, it turns out that the
choices we've made allow us to update our program with a minimum of fuss. We
only need to make a couple of changes:

  - Update argument handling to allow multiple files
  - Generate a status bar per-file, so we can show the right metadata
  - Paginate each file individually, but scroll through the full set of pages

Updating our program to support multiple arguments is pretty
straightforward. Previously, we checked to see if a user passed in more than one
argument, and if so we displayed an error. Now, we can remove the
check. Depending on the order you've worked through the exercises, you may or
may not have added some more robust error handling, or done other refactoring to
your version of hcat. In any case, you probably have a function that looks
something like this:

```haskell
handleArgs :: IO FilePath
handleArgs = do
  args <- getArgs
  case args of
    [] -> ioError $ userError "no filename provided"
    [fname] -> pure fname
    _ -> ioError $ userError "multiple files not supported"
```

The simplest refactor changes the type and makes a couple of minor changes to
the patterns in our `case` expression:

```haskell
handleArgs :: IO [FilePath]
handleArgs = do
  args <- getArgs
  case args of
    [] -> ioError $ userError "no filename provided"
    fnames -> pure fnames
```

This is a perfectly fine way to write our progrma, but our `case` expression is
only actually performing a single test to see if the argument list is
empty. That means we could also write this with an `if` expression:

```haskell
handleArgs :: IO [FilePath]
handleArgs = do
  args <- getArgs
  if null args
  then ioError $ userError "no filename provided"
  else pure args
```

Or, we can use the `when` function from `Control.Monad`:

```haskell
handleArgs :: IO [FilePath]
handleArgs = do
  args <- getArgs
  when (null args) $
    ioError $ userError "no filename provided"
  pure args
```

Ultimately, all of these approaches to writing `handleArgs` are valid. You can
choose whichever approach you find the most readable. No matter which one you
choose, you'll be left with a version of `handleArgs` that returns a list of
filenames. This is a change from the earlier version that only returned a single
filename, and it means we're going to need to make some additional changes in
our program to handle this new version of our function.

In earlier versions of this program, when we were only dealing with a single
file at a time, you probably had a function that looked something like this:

```haskell
runHCat :: IO ()
runHCat = do
  -- Get info about the terminal
  termSize <- getTerminalSize

  -- Get the contents of our file, and info about it
  fname <- handleArgs
  contents <- TextIO.readFile fname
  info <- fileInfo fname

  -- Paginate content, with info
  let pages = paginate termSize info contents

  -- show the pages of content, one at a time
  showPages pages
```

Now that we're getting a list of files, instead of a single file, we'll need to
rethink things a bit. Ultimately, we still want to show a list of pages, but now
the pages we're showing will come from several files, not just a single
file. Not only that, each of the pages needs to contain info about the specific
file that page came from.

One option we have is to simply refactor our program to deal with lists of both
file contents and file infos. You can attempt this refactor if you'd like, but
it's a bit unsatisfying so we'll look into an alternative.

The first thing we'll do is create a new type that can store a file along with
its metadata:

```haskell
data HCatFile = HCatFile
  { hcatFileInfo :: FileInfo
  , hcatFileContents :: Text
  }
```

Next, let's add a function that will fetch both the contents of the file and
it's metadata given it's path:

```haskell
getFileWithInfo :: FilePath -> IO HCatFile
getFileWithInfo filePath = do
  contents <- TextIO.readFile filePath
  info <- fileInfo filePath
  pure HCatFile
    { hcatFileInfo = info
    , hcatFileContents = contents
    }
```

Next, let's update our `paginate` function to work with a `HCatFile` instead of
taking contents and file info as ordinary arguments. Before our refactor, your
implementation probably looked something like this:

```haskell
paginate :: ScreenDimensions -> FileInfo -> Text -> [Text.Text]
paginate (ScreenDimensions rows cols) finfo contents =
  -- .. the rest of the function
```

Thankfully, we can refactor this function to use an `HCatFile` without needing
to change the body of the function at all. We'll just change the type, and
replace the `finfo` and `contents` arguments with a pattern match:

```haskell
paginate :: ScreenDimensions -> HCatFile -> [Text.Text]
paginate (ScreenDimensions rows cols) (HCatFile finfo contents) =
  -- .. the rest of the function
```

The last thing we'll need to do is update `runHCat` to handle our new code:

```haskell
runHCat :: IO ()
runHCat = do
  hSetBuffering stdout NoBuffering
  targetFiles <- traverse getFileWithInfo =<< handleArgs
  termSize <- getTerminalSize
  showPages $ concatMap (paginate termSize) targetFiles
```

In this version of our code, we're using `traverse` to get an `HCatFile` for
each argument passed in on the command line. The `concatMap` function generates
a list of pages from each file, and concatenates all of the pages together. This
has a nice side benefit of preserving the extra padding that `paginate` adds to
the end of the last page in a file, giving us a nice visual indicator that we're
about to move from one file to the next.

<br/>

<details>
<summary>Click here to see the the full program</summary>
```haskell
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
module EffectiveHaskell.Exercises.Chapter8.HCatMultiFile (runHCat) where

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

escapeInvertText :: Text.Text -> Text.Text
escapeInvertText inputStr =
  let
    reverseVideo = "\^[[7m"
    resetVideo = "\^[[0m"
  in reverseVideo <> inputStr <> resetVideo

formatFileInfo :: FileInfo -> Int -> Int -> Int -> Text.Text
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

wordWrap :: Int -> Text.Text -> [Text.Text]
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

showPages :: [Text.Text] -> IO ()
showPages [] = return ()
showPages (page:pages) = do
  clearScreen
  TextIO.putStr page
  nextStep <- getContinue
  case nextStep of
    Continue -> showPages pages
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


</div>
</div>
</details>

</div>
