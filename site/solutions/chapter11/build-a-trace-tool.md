---
chapter: 11
exercise-id: 3
name: Building A Trace Tool
summary: "
Summary TBD
"
---

## Building A Trace Tool {.problem}

In this chapter we focused on building a file archival tool, but many of the
ideas you learned here are applicable to other domains as well. Another area
you could use existential types is in building a tool to collect a trace of
function calls.

Consider this example:

```haskell
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE RecordWildCards #-}
module CallTrace where
import Text.Printf

data TraceData -- fill me in
newtype Trace -- fill me in

emptyTrace :: Trace
emptyTrace = undefined

traceCall
  :: (Show a, Show b)
  => String
  -> (a -> (Trace, b))
  -> a
  -> (Trace, b)
traceCall = undefined

showTrace :: Trace -> String
showTrace = undefined

factor :: Int -> (Trace, [Int])
factor n =
  traceCall "factor" factor' (n, 2)
  where
    factor' :: (Int, Int) -> (Trace, [Int])
    factor' (num, curFact)
      | num == 1 = (emptyTrace, [])
      | (num `mod` curFact) == 0 =
        let nextNumber = num `div` curFact
            message = "consFactor " <> show curFact
            (trace, results) = traceCall message factor' (nextNumber, curFact)
         in (trace, curFact : results)
      | otherwise =
        let nextFactor = curFact + 1
         in traceCall "skipFactor" factor' (num, nextFactor)

verboseFactor :: Int -> IO ()
verboseFactor n = do
  let (trace, factors) = factor n
  putStrLn "factors: "
  print factors
  putStrLn "trace: "
  putStrLn (showTrace trace)
```

Try to implement the missing pieces so that your program compiles and gives you
some appropriate output. Here are a couple of examples so that you can test your
own code:

```
λ verboseFactor 3
factors:
[3]
trace:
stack depth: 3
factor (3,2) => [3]
  skipFactor (3,3) => [3]
    consFactor 3 (1,3) => []

λ verboseFactor 1080
factors:
[2,2,2,3,3,3,5]
trace:
stack depth: 11
factor (1080,2) => [2,2,2,3,3,3,5]
  consFactor 2 (540,2) => [2,2,3,3,3,5]
    consFactor 2 (270,2) => [2,3,3,3,5]
      consFactor 2 (135,2) => [3,3,3,5]
        skipFactor (135,3) => [3,3,3,5]
          consFactor 3 (45,3) => [3,3,5]
            consFactor 3 (15,3) => [3,5]
              consFactor 3 (5,3) => [5]
                skipFactor (5,4) => [5]
                  skipFactor (5,5) => [5]
                    consFactor 5 (1,5) => []
```


### Hint 1 {.hint}

Some high level hint text

### Hint 2 {.hint}

Some more detailed hint text

### Hint 3 {.hint}

Even more detailed hint text

### Solution {.solution}

A complete solution for the exercise
