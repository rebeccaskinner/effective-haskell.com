module EffectiveHaskell.Exercises.Chapter7.Sequence where

sequenceIO :: [IO a] -> IO [a]
sequenceIO [] = return []
-- sequenceIO (x:xs) = x >>= \x' -> (x' :) <$> sequenceIO xs
-- sequenceIO (x:xs) = x >>= \x' -> sequenceIO xs >>= \xs' -> return (x':xs')
-- sequenceIO (x:xs) = do
--   x' <- x
--   xs' <- sequenceIO xs
--   return $ x' : xs'
