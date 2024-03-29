module EffectiveHaskell.Exercises.Chapter7.IOTypes where

doubleIO :: IO (IO String)
doubleIO = return $ return "hello"

returnRead :: IO (IO String)
returnRead = return $ readFile "/tmp/example"

printNestedIO :: IO (IO String) -> IO ()
printNestedIO nestedIO = nestedIO >>= go
  where
    go :: IO String -> IO ()
    go ioString = ioString >>= putStrLn
