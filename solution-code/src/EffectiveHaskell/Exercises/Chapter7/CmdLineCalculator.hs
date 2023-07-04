module EffectiveHaskell.Exercises.Chapter7.CmdLineCalculator where
import System.Environment (getArgs)

runCalculator :: IO ()
runCalculator = do
  args <- getArgs
  case args of
    [] -> putStrLn argsError
    [_] -> putStrLn argsError
    (op:numStrs) ->
      case getOperation op of
        Just f ->
          let nums = map read numStrs
          in print $ f nums
        Nothing -> putStrLn $ opError op
  where
    argsError =
      "Missing arg(s). Need an operator and at least 1 number"
    opError op =
      op <> " - Unrecognized Operator. Please use one of +,*,-,/"
    getOperation op =
      case op of
        "+" -> Just sum
        "*" -> Just product
        "-" -> Just $ foldl1 (-)
        "/" -> Just $ foldl1 div
        _ -> Nothing

-- printArgs :: IO ()
-- printArgs = do
--   (first:rest) <- getArgs
--   putStrLn $ "first: " <> first
--   putStrLn $ "rest: " <> show rest
