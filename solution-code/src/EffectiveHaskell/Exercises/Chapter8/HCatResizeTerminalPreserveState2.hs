{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
module EffectiveHaskell.Exercises.Chapter8.HCatResizeTerminalPreserveState2 (runHCat) where

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
  } deriving Show

data Page = Page
  { pageContents :: Text
  , pageFooter :: Text
  , pageOffset :: Int
  , documentContents :: Text
  }

wordWrap :: Int -> Text -> [(Int, Text)]
wordWrap = go 0
  where
    go startingColumn lineLength lineText
      | Text.length lineText <= lineLength = [(startingColumn, lineText)]
      | otherwise =
        let
          (candidate, nextLines) = Text.splitAt lineLength lineText
          (firstLine, overflow) = softWrap candidate (Text.length candidate - 1)
          nextStart = startingColumn + Text.length firstLine
        in (startingColumn, firstLine) : go nextStart lineLength (overflow <> nextLines)
      where
        softWrap hardwrappedText textIndex
          | textIndex <= 0 = (hardwrappedText,Text.empty)
          | Text.index hardwrappedText textIndex == ' ' =
            let (wrappedLine, rest) = Text.splitAt textIndex hardwrappedText
            in (wrappedLine, Text.tail rest)
          | otherwise = softWrap hardwrappedText (textIndex - 1)

paginateWith :: ScreenDimensions -> (Int -> Int -> Int -> Text) -> Text -> [Page]
paginateWith (ScreenDimensions rows cols) formatStatusBar contents =
  let
    rows' = rows - 1
    wrappedLines = concatMap (wordWrap cols) (Text.lines contents)
    pageData = groupPageLines rows' wrappedLines
    pageCount = length pageData
    statusLines = map (formatStatusBar cols pageCount) [1..pageCount]
  in zipWith (mkPage rows') pageData statusLines
  where
    pageDataWithOffset :: [(Int, Text)] -> (Int, [Text])
    pageDataWithOffset [] = (0, [])
    pageDataWithOffset pageLines =
      (minimum $ map fst pageLines, map snd pageLines)

    groupPageLines :: Int -> [(Int, Text)] -> [(Int, [Text])]
    groupPageLines pageSize = map pageDataWithOffset . groupsOf pageSize

    mkPage :: Int -> (Int, [Text]) -> Text -> Page
    mkPage pageSize (offset, pageLines) footer =
      Page
        { pageContents = Text.unlines $ padTo pageSize pageLines
        , pageFooter = footer
        , pageOffset = offset
        , documentContents = contents
        }

    padTo :: Int -> [Text] -> [Text]
    padTo lineCount rowsToPad =
      if length rowsToPad >= lineCount
      then rowsToPad
      else rowsToPad <> replicate (lineCount - length rowsToPad) "~"

paginate :: ScreenDimensions -> HCatFile -> [Page]
paginate terminalSize (HCatFile finfo contents) =
  paginateWith terminalSize (formatFileInfo finfo) contents

paginateHelpText :: ScreenDimensions -> Text -> [Page]
paginateHelpText terminalSize =
  paginateWith terminalSize helpStatusBar
  where
    helpStatusBar cols _ _ =
      Text.take cols $ "Help Info: Press 'q' to exit" <> Text.replicate cols " "

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
  | ViewHelp
  deriving (Eq, Show)

data ViewState = ViewState
  { originalFiles :: [HCatFile]
  , currentPages :: ZipList Page
  , paginatedFiles :: ZipList Page
  , currentMode :: ViewMode
  }

viewStateNextPage :: ViewState -> ViewState
viewStateNextPage viewState =
  viewState { currentPages = ZipList.next $ currentPages viewState }

viewStatePreviousPage :: ViewState -> ViewState
viewStatePreviousPage viewState =
  viewState { currentPages = ZipList.prev $ currentPages viewState }

viewStateToHelp :: ViewState -> IO ViewState
viewStateToHelp viewState = do
  case currentMode viewState of
    ViewHelp -> pure viewState
    ViewContent -> do
      termSize <- getTerminalSize
      pure $ viewState
        { currentPages = unsafeZipList $ paginateHelpText termSize helpText
        , paginatedFiles = currentPages viewState
        }

viewStateIsHelp :: ViewState -> Bool
viewStateIsHelp = (== ViewHelp) . currentMode

viewStateIsDone :: ViewState -> Bool
viewStateIsDone = ZipList.isEnd . currentPages

viewStateResize' :: ViewState -> IO (ScreenDimensions, ViewState)
viewStateResize' viewState = do
  termSize <- getTerminalSize
  let
    content = unsafeZipList $ concatMap (paginate termSize) (originalFiles viewState)
    help = unsafeZipList $ paginateHelpText termSize helpText
    current = if viewStateIsHelp viewState then help else content
  pure (termSize, viewState { paginatedFiles = content, currentPages = current })

viewStateResize :: ViewState -> IO ViewState
viewStateResize = fmap snd . viewStateResize'

showPages :: [HCatFile] -> IO ()
showPages inputFiles = do
  termSize <- getTerminalSize
  let
    pages = unsafeZipList $ concatMap (paginate termSize) inputFiles
    viewState =
      ViewState
        { originalFiles = inputFiles
        , currentPages = pages
        , paginatedFiles = pages
        , currentMode = ViewContent
        }
  showForwardBack viewState

printPage :: Page -> IO ()
printPage page =
  TextIO.putStr (pageContents page <> pageFooter page)

showForwardBack :: ViewState -> IO ()
showForwardBack viewState = do
  clearScreen
  printPage $ ZipList.value (currentPages viewState)
  nextStep <- getInput
  case nextStep of
    PageNext ->
      if viewStateIsDone viewState
      then pure ()
      else showForwardBack $ viewStateNextPage viewState
    PagePrevious ->
      showForwardBack $ viewStatePreviousPage viewState
    HelpScreen -> do
      viewStateToHelp viewState >>= showForwardBack
      viewStateResize viewState >>= showForwardBack
    ResizeTerminal -> do
      (termSize, viewState') <- viewStateResize' viewState
      let
        currentOffset = pageOffset . ZipList.value . currentPages $ viewState
        documentTail = Text.drop currentOffset . documentContents . ZipList.value . currentPages $ viewState
        -- paginateTempPage = paginateWith termSize (\_ _ _ -> "viewport resized")
        paginateTempPage = paginateWith termSize (\_ _ _ -> "viewport resized; currentOffset: " <> tshow currentOffset)
        currentPage = ZipList.singleton . head . paginateTempPage $ documentTail
        tempViewstate = viewState' { currentPages = currentPage }
      showForwardBack tempViewstate >> showForwardBack viewState'
      -- if ZipList.isStart (currentPages viewState')
      --   then showForwardBack viewState'
      --   else showForwardBack tempViewstate >> showForwardBack viewState'
    Cancel -> pure ()

tshow :: Show a => a -> Text
tshow = Text.pack . show

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
