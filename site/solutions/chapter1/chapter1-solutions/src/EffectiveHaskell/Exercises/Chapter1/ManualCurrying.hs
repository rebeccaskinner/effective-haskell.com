module EffectiveHaskell.Exercises.Chapter1.ManualCurrying
  ( exampleCurry
  , exampleUncurry
  )
where

exampleCurry :: ((a,b) -> c) -> a -> b -> c
exampleCurry f a b = f (a,b)

exampleUncurry :: (a -> b -> c) -> (a,b) -> c
exampleUncurry f argTuple= f (fst argTuple) (snd argTuple)
