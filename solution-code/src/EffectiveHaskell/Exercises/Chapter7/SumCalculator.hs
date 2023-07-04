module EffectiveHaskell.Exercises.Chapter7.SumCalculator where
import System.Environment (getArgs)

runBind :: IO ()
runBind = getArgs >>= showSum
  where
    sumInputs inputs = sum $ map read inputs
    showSum inputs = print $ sumInputs inputs
-- runBind = getArgs >>= printArgs
--   where
--     addArgs :: [String] -> Int
--     addArgs = sum . map read
--     printArgs = print . addArgs


runDo :: IO ()
runDo = do
  result <- sumInputs <$> getArgs
  print result
  where
    sumInputs = sum . map read

-- printArgs :: IO ()
-- printArgs = do
--   (first:rest) <- getArgs
--   putStrLn $ "first: " <> first
--   putStrLn $ "rest: " <> show rest
