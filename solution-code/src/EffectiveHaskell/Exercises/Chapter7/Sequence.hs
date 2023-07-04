module EffectiveHaskell.Exercises.Chapter7.Sequence where

sequenceIO :: [IO a] -> IO [a]
sequenceIO [] = return []
sequenceIO (x:xs) = do
  x' <- x
  xs' <- sequenceIO xs
  return $ x' : xs'

-- sequenceIO (x:xs) = x >>= \x' -> rest >>= \rest' -> return $ x' : rest'
-- sequenceIO (x:xs) = rest >>= \rest' -> x >>= \x' -> return $ x' : rest'
--   where rest = sequenceIO xs
-- sequenceIO (x:xs) = x >>= \x' -> sequenceIO xs >>= \xs' -> return (x':xs')
-- sequenceIO (x:xs) = x >>= \x' -> (x' :) <$> sequenceIO xs
-- sequenceIO (x:xs) = x >>= \x' -> sequenceIO xs >>= \xs' -> return (x':xs')

readFilesFromFile :: FilePath -> IO String
readFilesFromFile inputFile = do
  inputContents <- readFile inputFile
  let filesToRead = lines inputContents
  -- allFileContents <- sequenceIO $ map readFile filesToRead
  allFileContents <- traverseIO readFile filesToRead
  return $ unlines allFileContents

traverseIO :: (a -> IO b) -> [a] -> IO [b]
traverseIO f as = sequenceIO $ map f as

sequenceIO_ :: [IO a] -> IO ()
sequenceIO_ actions = sequenceIO actions >> return ()
