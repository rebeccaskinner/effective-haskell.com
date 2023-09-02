{-# LANGUAGE OverloadedStrings #-}
module EffectiveHaskell.Exercises.Chapter8.CountWordsByteString where
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BS
import Data.Char

addWord :: ByteString -> [(ByteString, Int)] -> [(ByteString, Int)]
addWord targetWord [] = [(targetWord, 1)]
addWord targetWord ((thisWord, thisCount) : rest)
  | targetWord == thisWord = (thisWord, thisCount + 1) : rest
  | otherwise = (thisWord, thisCount) : addWord targetWord rest

countWords :: ByteString -> [(ByteString, Int)]
countWords =
  foldr addWord [] .
  dropShortWords .
  BS.words .
  normalize
  where
    dropShortWords = filter (\w -> BS.length w >= 3)
    normalize = BS.map (dropPunctuation . toLower)
    dropPunctuation char
      | isLetter char || isNumber char = char
      | otherwise = ' '

showWordCounts :: [(ByteString, Int)] -> ByteString
showWordCounts wordCounts = BS.unlines $ map showWordCount wordCounts
  where
    maxWordLength = maximum [BS.length word | (word, _) <- wordCounts]
    paddingLength = maxWordLength + 4
    padding = BS.replicate paddingLength '.'
    wordWithPadding word = BS.take paddingLength $ word <> padding
    maxNumLength = maximum $ map (BS.length . BS.pack . show . snd) wordCounts
    numPadding = BS.replicate maxNumLength '.'
    rightAlignNum num = BS.reverse . BS.take maxNumLength $ BS.reverse (BS.pack $ show num) <> numPadding
    showWordCount (word, count) =
      wordWithPadding word <> rightAlignNum count

showFileCount :: FilePath -> IO ()
showFileCount fileName = BS.readFile fileName >>= BS.putStrLn . showWordCounts . countWords
