{-# LANGUAGE OverloadedStrings #-}
module EffectiveHaskell.Exercises.Chapter8.CountWordsText where
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import Data.Char

addWord :: Text -> [(Text, Int)] -> [(Text, Int)]
addWord targetWord [] = [(targetWord, 1)]
addWord targetWord ((thisWord, thisCount) : rest)
  | targetWord == thisWord = (thisWord, thisCount + 1) : rest
  | otherwise = (thisWord, thisCount) : addWord targetWord rest

countWords :: Text -> [(Text, Int)]
countWords =
  foldr addWord [] .
  dropShortWords .
  Text.words .
  normalize
  where
    dropShortWords = filter (\w -> Text.length w >= 3)
    normalize = Text.map (dropPunctuation . toLower)
    dropPunctuation char
      | isLetter char || isNumber char = char
      | otherwise = ' '

showWordCounts :: [(Text, Int)] -> Text
showWordCounts wordCounts = Text.unlines $ map showWordCount wordCounts
  where
    maxWordLength = maximum [Text.length word | (word, _) <- wordCounts]
    paddingLength = maxWordLength + 4
    padding = Text.replicate paddingLength "."
    wordWithPadding word = Text.take paddingLength $ word <> padding
    maxNumLength = maximum $ map (Text.length . Text.pack . show . snd) wordCounts
    numPadding = Text.replicate maxNumLength "."
    rightAlignNum num = Text.reverse . Text.take maxNumLength $ Text.reverse (Text.pack $ show num) <> numPadding
    showWordCount (word, count) =
      wordWithPadding word <> rightAlignNum count

showFileCount :: FilePath -> IO ()
showFileCount fileName = Text.readFile fileName >>= Text.putStrLn . showWordCounts . countWords
