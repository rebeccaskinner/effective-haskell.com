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
