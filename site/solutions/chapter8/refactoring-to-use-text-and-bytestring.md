---
chapter: 8
exercise-id: 3
name: Refactoring to use Text and Bytestring
summary: "
This exercise will help you get more experience dealing with different string
types in Haskell. You'll also get more hands on experience refactoring between
different string representations.
"
---

## Refactoring to use Text and Bytestring {.problem}

Many of the earlier exercises in this book used `String` instead of `ByteString`
or `Text`. Refactor some of your existing code to use these more efficient types
instead.


### Hints
<div class="hints">

<details>
<summary>Click to reveal</summary>
<div class="details-body-outer">
<div class="details-body">

There are strict and lazy variants of both `Text` and `ByteString`. The strict
versions are available in `Data.Text` and `Data.ByteString` while the lazy
versions are in `Data.Text.Lazy` and `Data.ByteString.Lazy`. We'll use the
strict variants in these examples, but the strict and lazy versions typically
provide compatible interfaces, so it's not a big change to reafctor from one to
the other.

</div>
</div>
</details>

<details>
<summary>Click to reveal</summary>
<div class="details-body-outer">
<div class="details-body">

The `bytestring` package provides most of the usual IO functions for reading and
writing `ByteString` values in `Data.ByteString` (or `Data.ByteString.Lazy` for
the lazy versions). The `text` package, on the other hand, puts them into the
`Data.Text.IO` module.

When dealing with `Text` it's often convenient to import both `Data.Text` and
`Data.Text.IO` under the same name:

```haskell
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
```

</div>
</div>
</details>

<details>
<summary>Click to reveal</summary>
<div class="details-body-outer">
<div class="details-body">

Although it's common to use `ByteString` values to deal with text data, they are
intended to support any arbitrary binary data. The `Data.ByteString.Char8`
module provides some extra utlities for working with text data. Be careful
though, since it doesn't enforce any particular encoding. You can use
`Data.ByteString.Char8` to create invalid unicode strings.

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

The purpose of this exercise is to help you get experience refactoring your
programs to use different types of string data. There isn't a single right
answer to this problem since it's not a single question, but let's look at a
characteristic example.

Imagine we want to write a program that counts how many times each word appears
in a file, and then prints out a summary. We might start using `String`:

```haskell
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
```

Next, let's refactor this program to use `ByteString`:

```haskell
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
```

You'll notice at a quick glance that these two implements are very similar-
similar enough it might be hard to spot all the differences. Let's diff them:

<details>
<summary>Expand to see text diff</summary>
```
1c1,4
< module EffectiveHaskell.Exercises.Chapter8.CountWordsString where
---
> {-# LANGUAGE OverloadedStrings #-}
> module EffectiveHaskell.Exercises.Chapter8.CountWordsByteString where
> import Data.ByteString (ByteString)
> import qualified Data.ByteString.Char8 as BS
4c7
< addWord :: String -> [(String, Int)] -> [(String, Int)]
---
> addWord :: ByteString -> [(ByteString, Int)] -> [(ByteString, Int)]
10c13
< countWords :: String -> [(String, Int)]
---
> countWords :: ByteString -> [(ByteString, Int)]
14c17
<   words .
---
>   BS.words .
17,18c20,21
<     dropShortWords = filter (\w -> length w >= 3)
<     normalize = map (dropPunctuation . toLower)
---
>     dropShortWords = filter (\w -> BS.length w >= 3)
>     normalize = BS.map (dropPunctuation . toLower)
23,24c26,27
< showWordCounts :: [(String, Int)] -> String
< showWordCounts wordCounts = unlines $ map showWordCount wordCounts
---
> showWordCounts :: [(ByteString, Int)] -> ByteString
> showWordCounts wordCounts = BS.unlines $ map showWordCount wordCounts
26c29
<     maxWordLength = maximum [length word | (word, _) <- wordCounts]
---
>     maxWordLength = maximum [BS.length word | (word, _) <- wordCounts]
28,32c31,35
<     padding = replicate paddingLength '.'
<     wordWithPadding word = take paddingLength $ word <> padding
<     maxNumLength = maximum $ map (length . show . snd) wordCounts
<     numPadding = replicate maxNumLength '.'
<     rightAlignNum num = reverse . take maxNumLength $ reverse (show num) <> numPadding
---
>     padding = BS.replicate paddingLength '.'
>     wordWithPadding word = BS.take paddingLength $ word <> padding
>     maxNumLength = maximum $ map (BS.length . BS.pack . show . snd) wordCounts
>     numPadding = BS.replicate maxNumLength '.'
>     rightAlignNum num = BS.reverse . BS.take maxNumLength $ BS.reverse (BS.pack $ show num) <> numPadding
37c40
< showFileCount fileName = readFile fileName >>= putStrLn . showWordCounts . countWords
---
> showFileCount fileName = BS.readFile fileName >>= BS.putStrLn . showWordCounts . countWords
```
</details>
![a screen shot showing the differences between the String and ByteString implementations of a word count tool](/images/solutions/chapter8/count-words-string-bytestring-diff.webp)

As you can see from the diff, the changes we've made to refactor our code to use
a `ByteString` instead of a `String` are minimal. We've changed `String` to
`ByteString` in the type annotations for our top-level functions, and we've
replaced calls to list functions in `Prelude` like `reverse` and `replicate`
with the equivalent functions in `Data.ByteString`.

Let's refactor this program one more time, this time to use `Text` instead of
`ByteString`:

```haskell
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
```

And, like before, let's look at the diff:

<details>
<summary>Expand to see text diff</summary>
```
2,4c2,5
< module EffectiveHaskell.Exercises.Chapter8.CountWordsByteString where
< import Data.ByteString (ByteString)
< import qualified Data.ByteString.Char8 as BS
---
> module EffectiveHaskell.Exercises.Chapter8.CountWordsText where
> import Data.Text (Text)
> import qualified Data.Text as Text
> import qualified Data.Text.IO as Text
7c8
< addWord :: ByteString -> [(ByteString, Int)] -> [(ByteString, Int)]
---
> addWord :: Text -> [(Text, Int)] -> [(Text, Int)]
13c14
< countWords :: ByteString -> [(ByteString, Int)]
---
> countWords :: Text -> [(Text, Int)]
17c18
<   BS.words .
---
>   Text.words .
20,21c21,22
<     dropShortWords = filter (\w -> BS.length w >= 3)
<     normalize = BS.map (dropPunctuation . toLower)
---
>     dropShortWords = filter (\w -> Text.length w >= 3)
>     normalize = Text.map (dropPunctuation . toLower)
26,27c27,28
< showWordCounts :: [(ByteString, Int)] -> ByteString
< showWordCounts wordCounts = BS.unlines $ map showWordCount wordCounts
---
> showWordCounts :: [(Text, Int)] -> Text
> showWordCounts wordCounts = Text.unlines $ map showWordCount wordCounts
29c30
<     maxWordLength = maximum [BS.length word | (word, _) <- wordCounts]
---
>     maxWordLength = maximum [Text.length word | (word, _) <- wordCounts]
31,35c32,36
<     padding = BS.replicate paddingLength '.'
<     wordWithPadding word = BS.take paddingLength $ word <> padding
<     maxNumLength = maximum $ map (BS.length . BS.pack . show . snd) wordCounts
<     numPadding = BS.replicate maxNumLength '.'
<     rightAlignNum num = BS.reverse . BS.take maxNumLength $ BS.reverse (BS.pack $ show num) <> numPadding
---
>     padding = Text.replicate paddingLength "."
>     wordWithPadding word = Text.take paddingLength $ word <> padding
>     maxNumLength = maximum $ map (Text.length . Text.pack . show . snd) wordCounts
>     numPadding = Text.replicate maxNumLength "."
>     rightAlignNum num = Text.reverse . Text.take maxNumLength $ Text.reverse (Text.pack $ show num) <> numPadding
40c41
< showFileCount fileName = BS.readFile fileName >>= BS.putStrLn . showWordCounts . countWords
---
> showFileCount fileName = Text.readFile fileName >>= Text.putStrLn . showWordCounts . countWords
```
</details>

![a screen shot showing the differences between the ByteString and Text
implementations of a word count tool](/images/solutions/chapter8/count-words-bytestring-text-diff.webp)

Once again, you can see that the changes we needed to make were minimal.

</div>
</div>
</details>

</div>
