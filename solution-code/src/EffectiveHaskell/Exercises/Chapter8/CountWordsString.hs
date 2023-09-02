module EffectiveHaskell.Exercises.Chapter8.CountWordsString where
import Data.Char

addWord :: String -> [(String, Int)] -> [(String, Int)]
addWord targetWord [] = [(targetWord, 1)]
addWord targetWord ((thisWord, thisCount) : rest)
  | targetWord == thisWord = (thisWord, thisCount + 1) : rest
  | otherwise = (thisWord, thisCount) : addWord targetWord rest

countWords :: String -> [(String, Int)]
countWords =
  foldr addWord [] .
  dropShortWords .
  words .
  normalize
  where
    dropShortWords = filter (\w -> length w >= 3)
    normalize = map (dropPunctuation . toLower)
    dropPunctuation char
      | isLetter char || isNumber char = char
      | otherwise = ' '

showWordCounts :: [(String, Int)] -> String
showWordCounts wordCounts = unlines $ map showWordCount wordCounts
  where
    maxWordLength = maximum [length word | (word, _) <- wordCounts]
    paddingLength = maxWordLength + 4
    padding = replicate paddingLength '.'
    wordWithPadding word = take paddingLength $ word <> padding
    maxNumLength = maximum $ map (length . show . snd) wordCounts
    numPadding = replicate maxNumLength '.'
    rightAlignNum num = reverse . take maxNumLength $ reverse (show num) <> numPadding
    showWordCount (word, count) =
      wordWithPadding word <> rightAlignNum count

showFileCount :: FilePath -> IO ()
showFileCount fileName = readFile fileName >>= putStrLn . showWordCounts . countWords
