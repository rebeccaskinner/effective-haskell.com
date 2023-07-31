module Main where
import System.Environment

data Config = Config
  { configInputFile :: FilePath
  , configNeedle :: String
  , configReplacement :: String
  }

replaceTargetInDocument :: String -> String -> String -> String
replaceTargetInDocument needle replacement =
  unlines . map replaceInLine . lines
  where
    replaceInLine = unwords . map replaceTargetWith . words
    replaceTargetWith input
      | needle == input = replacement
      | otherwise = input

getConfig :: IO (Maybe Config)
getConfig = do
  args <- getArgs
  case args of

  [path, needle, replacement] <- getArgs
  return $ Config path needle replacement

runConfig :: Config -> IO String
runConfig (Config path needle replacement) = do
  document <- readFile path
  return $ replaceTargetInDocument needle replacement document

main :: IO ()
main = getConfig >>= runConfig >>= putStrLn
