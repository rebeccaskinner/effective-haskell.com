{-# LANGUAGE TypeApplications #-}
module EffectiveHaskell.Exercises.Chapter8.ReadError where
import Control.Exception

readWithCatch :: String -> IO Int
readWithCatch input =
  catch @IOException readInput $ \_e -> pure 0
  where
    readInput = pure . read $ input

readWithCatch' :: String -> IO Int
readWithCatch' input =
  catch @ErrorCall readInput $ \_e -> pure 0
  where
    readInput = evaluate $ read input
