module EffectiveHaskell.Exercises.Chapter7.Join where

-- joinIO ioAction = ioAction >>= _
joinIO :: IO (IO a) -> IO a
joinIO ioAction = ioAction >>= id
-- joinIO ioAction = ioAction >>= returnInnerIO
--   where
--     returnInnerIO :: IO a -> IO a
--     returnInnerIO a = a
