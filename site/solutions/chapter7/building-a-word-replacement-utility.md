---
chapter: 7
exercise-id: 3
name: Building A Word Replacement Utility
summary: "
Summary TBD
"
---

## Building A Word Replacement Utility {.problem}

Write an application that will accept three arguments on the command line:

- `path`: The path to a file
- `needle`: A word to find in the input file
- `replacement`: A word to use as a replacement when printing the file

When a user runs your program, you should print out the contents of the file at
`path`, but replace all occurrences of `needle` with `replacement` in the
output. To make things easier, assume that you can use the `words` function and
don't need to worry about handling multiple spaces or words that span lines.

### Hints
<div class="hints">

<details>
<summary>Click to reveal</summary>
<div class="details-body-outer">
<div class="details-body">
You can use the `readFile` function to read the contents of a file:

```haskell
readFile :: FilePath -> IO String
```

Remember that `FilePath` is an alias for `String`, so you can pass any `String`
value to `readFile`.
</div>
</div>
</details>

<details>
<summary>Click to reveal</summary>
<div class="details-body-outer">
<div class="details-body">

There are four functions in `Prelude` that will be helpful as you work to
replace the words in a document:

```haskell
-- Converts a String into a list of words by splitting along spaces.
words :: String -> [String]

-- Converts a list of words into a String by joining them with spaces
unwords :: [String] -> String

-- Works like words, but it splits on newlines
lines :: String -> [String]

-- Works like unwords, but it joins the list with newlines
unlines :: [String] -> String
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

As you saw in the previous exercise, writing programs that use command line
arguments and deal with files can introduce a lot of additional error handling
that can detract from the core problem that we're trying to solve. This time,
let's focus on solving our problem without the extra error the handing.

We'll need to work with three different command line arguments for this
program. Let's start by creating a new record to hold our configuration data:

```haskell
data Config = Config
  { configInputFile :: FilePath
  , configNeedle :: String
  , configReplacement :: String
  }
```

Next, let's create a new IO action that will get the command line arguments and
use them to generate a `Config` record:

```haskell
getConfig :: IO Config
getConfig = do
  [path, needle, replacement] <- getArgs
  return $ Config path needle replacement
```

Now that we have a config, let's create another IO action to handle reading a
file and replacing the contents based on the current configuration. We'd like to
keep the pure code separate from the code with side effects, so we'll call a
not-yet-written function named `replaceTargetInDocument` that will do the actual
work of replacing the text. We'll implement that function soon, for now we'll
create a placeholder and leave it `undefined`.

```haskell
replaceTargetInDocument :: String -> String -> String -> String
replaceTargetInDocument = undefined

runConfig :: Config -> IO String
runConfig (Config path needle replacement) = do
  document <- readFile path
  return $ replaceTargetInDocument needle replacement document
```

Although `runConfig` does most of the heavy lifting, we'll still need a `main`
function to get the config, pass it into `runConfig`, and finally to print out
the results. Thanks to the way we've written these functions, we can
easily combine them with `(>>)`:

```haskell
main :: IO ()
main = getConfig >>= runConfig >>= putStrLn
```

We're getting close to a solution, but we still haven't can't quite test
this. Since we haven't defined `replaceTargetInDocument`, any attempt to test
our program will crash:

```haskell
λ withArgs ["/tmp/poem.txt", "George", "Echo"] main
*** Exception: Prelude.undefined
CallStack (from HasCallStack):
  error, called at libraries/base/GHC/Err.hs:74:14 in base:GHC.Err
  undefined, called at WordReplacement.hs:16:27 in solution-code-0-inplace:EffectiveHaskell.Exercises.Chapter7.WordReplacement
```

Let's finish the last bit of our program and then try again. We need to define
`replaceTargetInDocument`. This function will be responsible for replacing every
occurrence of the needle in a document with the replacement:

```haskell
replaceTargetInDocument :: String -> String -> String -> String
replaceTargetInDocument needle replacement =
  unwords . map replaceTargetWith . words
  where
    replaceTargetWith input
      | needle == input = replacement
      | otherwise = input
```

This function works by first taking the full document and converting it to a
list of individual words using the `words` function from `Prelude`. Next, for
each individual word, we check to see if the word matches `needle` and, if so,
replace it with `replacement`. Finally, we re-combine all of the
post-replacement words into a single string with the `unwords` function.

With this last function defined, we can test our new program. Let's give it a
try with a short poem:

<blockquote>
<p>
Once was a parrot, George by name,<br/>
Who played a quite unusual game.<br/>
A fervent coder, to our surprise,<br/>
In love with Haskell's neat disguise.<br/>
</p>
<br/>
<p>
"Good day," George squawks, takes his stance,<br/>
In lines of Haskell code, he'd dance.<br/>
From loops to functions, night and day,<br/>
In data types, George would play.<br/>
</p>
<br/>
<p>
George wasn't your typical bird,<br/>
His love for code, it was absurd.<br/>
"Skip the cracker, bring me scripts,<br/>
Watch my joy in coding flips!"<br/>
</p>
<br/>
<p>
George, oh George, so bright and clever,<br/>
In the world of bugs, he'd never waver.<br/>
His playground wasn't skies or trees,<br/>
But the logic of his machine's keys.<br/>
</p>
<br/>
<p>
"Give me Haskell," cries George in glee,<br/>
His feathers twitching with pure spree.<br/>
The joy of coding he implores,<br/>
Syntax sugar, he adores.<br/>
</p>
<br/>
<p>
So here's to George, with his might,<br/>
Coding Haskell, day and night.<br/>
Remember him when you hear a squawk,<br/>
It's George the Parrot, in code talk.<br/>
</p>
</blockquote>

Let's use our new program to try to replace `George` with `Echo` in the body of
our poem:

```
user@host:~WordReplacement$ ghc WordReplacement.hs
user@host:~WordReplacement$ ./WordReplacement ./poem.txt George Echo
Once was a parrot, Echo by name, Who played a quite unusual game. A fervent
coder, to our surprise, In love with Haskell's neat disguise. "Good day," Echo
squawks, takes his stance, In lines of Haskell code, he'd dance. From loops to
functions, night and day, In data types, Echo would play. Echo wasn't your
typical bird, His love for code, it was absurd. "Skip the cracker, bring me
scripts, Watch my joy in coding flips!" George, oh George, so bright and clever,
In the world of bugs, he'd never waver. His playground wasn't skies or trees,
But the logic of his machine's keys. "Give me Haskell," cries Echo in glee, His
feathers twitching with pure spree. The joy of coding he implores, Syntax sugar,
he adores. So here's to George, with his might, Coding Haskell, day and
night. Remember him when you hear a squawk, It's Echo the Parrot, in code talk.
```

Our program seems to be working pretty well, but our use of `words` and
`unwords` is causing us to lose newlines. Perfectly preserving formatting can
turn into a pretty complicated problem if we want to address all possible edge
cases, but let's take one more pass at a slightly more robust implementation of
our program. In our new version, we'll first split our program into lines, then
split each line into words. We'll lose extra spacing between words, but we'll
still be able to preserve newlines:

```haskell
replaceTargetInDocument :: String -> String -> String -> String
replaceTargetInDocument needle replacement =
  unlines . map replaceInLine . lines
  where
    replaceInLine = unwords . map replaceTargetWith . words
    replaceTargetWith input
      | needle == input = replacement
      | otherwise = input
```

As you can see, we only need to make a couple of minor changes to
`replaceTargetInDocument` to add support for retaining empty lines. Instead of
immediately breaking the entire document into words and calling
`replaceTargetWith`, we first break our document into lines. We take the same
approach for each line that we originally took for the whole document: break the
line into words, apply `replaceTargetWith` to each word, then rejoin the
docment. Let's try it out:

```
user@host:~WordReplacement$ ghc WordReplacement.hs
user@host:~WordReplacement$ ./WordReplacement ./poem.txt George Echo
Once was a parrot, Echo by name,
Who played a quite unusual game.
A fervent coder, to our surprise,
In love with Haskell's neat disguise.

"Good day," Echo squawks, takes his stance,
In lines of Haskell code, he'd dance.
From loops to functions, night and day,
In data types, Echo would play.

Echo wasn't your typical bird,
His love for code, it was absurd.
"Skip the cracker, bring me scripts,
Watch my joy in coding flips!"

George, oh George, so bright and clever,
In the world of bugs, he'd never waver.
His playground wasn't skies or trees,
But the logic of his machine's keys.

"Give me Haskell," cries Echo in glee,
His feathers twitching with pure spree.
The joy of coding he implores,
Syntax sugar, he adores.

So here's to George, with his might,
Coding Haskell, day and night.
Remember him when you hear a squawk,
It's Echo the Parrot, in code talk.
```

</div>
</div>
</details>

</div>
