{-# LANGUAGE OverloadedStrings #-}
module EffectiveHaskell.Exercises.Chapter8.HCatDoNotation (runHCat) where

import qualified Data.Text as Text
import qualified Data.Text.IO as TextIO
import qualified Data.ByteString as BS
import qualified System.Environment as Env
import qualified Control.Exception as Exception
import qualified System.IO.Error as IOError
import System.IO
import System.Process (readProcess)
import qualified System.Info as SystemInfo
import qualified Data.Time.Clock as Clock

data ContinueCancel = Continue | Cancel deriving (Eq, Show)

data FileInfo = FileInfo
  { filePath  :: FilePath
  , fileSize  :: Int
  , fileMTime :: Clock.UTCTime
  , fileReadable :: Bool
  , fileWriteable :: Bool
  , fileExecutable :: Bool
  } deriving Show

data ScreenDimensions = ScreenDimensions
  { screenRows :: Int
  , screenColumns :: Int
  } deriving (Eq, Show)

handleArgs :: IO (Either String FilePath)
handleArgs = do
  args <- Env.getArgs
  pure $
    case args of
      [fname] -> Right fname
      []      -> Left "no filename provided"
      _       -> Left "multiple files not supported"

eitherToErr :: Show a => Either a b -> IO b
eitherToErr (Right a) = pure a
eitherToErr (Left e) = Exception.throwIO . IOError.userError $ show e

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

paginate :: ScreenDimensions -> Text.Text -> [Text.Text]
paginate (ScreenDimensions rows cols) text =
  let unwrappedLines = Text.lines text
      wrappedLines = concatMap (wordWrap cols) unwrappedLines
      pageLines = groupsOf rows wrappedLines
  in map Text.unlines pageLines

getTerminalSize :: IO ScreenDimensions
getTerminalSize =
  case SystemInfo.os of
    "darwin" -> tputScreenDimensions
    "linux" -> tputScreenDimensions
    _other -> pure $ ScreenDimensions 25 80
  where
    tputScreenDimensions :: IO ScreenDimensions
    tputScreenDimensions = do
      termLines <- readProcess "tput" ["lines"] ""
      cols <- readProcess "tput" ["cols"] ""
      let
        termLines' = read $ init termLines
        cols' = read $ init cols
      pure $ ScreenDimensions termLines' cols'

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
showPages [] = pure ()
showPages (page:pages) = do
  clearScreen
  TextIO.putStr page
  input <- getContinue
  case input of
    Continue -> showPages pages
    Cancel   -> pure ()

clearScreen :: IO ()
clearScreen = BS.putStr "\^[[1J\^[[1;1H"

runHCat :: IO ()
runHCat = do
  args <- handleArgs
  fname <- eitherToErr args
  handle <- openFile fname ReadMode
  contents <- TextIO.hGetContents handle
  termSize <- getTerminalSize
  let pages = paginate termSize contents
  showPages pages
