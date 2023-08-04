module EffectiveHaskell.Exercises.Chapter8.UpdatedGetTerminalSize where
import qualified Data.Text.IO as TextIO
import qualified System.Info as SystemInfo
import System.Process (readProcess)
import Control.Exception (IOException, catch)
import System.IO
import qualified Data.Text as Text
import qualified System.Environment as Environment
import qualified Control.Exception as Exception
import qualified System.IO.Error as IOError
import Text.Read

data FileInfo
data ScreenDimensions = ScreenDimensions
  { screenRows :: Int
  , screenColumns :: Int
  } deriving (Eq, Show)


getTerminalSize :: IO ScreenDimensions
getTerminalSize =
  case SystemInfo.os of
    "darwin" -> tputScreenDimensions
    "linux" -> tputScreenDimensions
    _other -> pure $ ScreenDimensions 25 80
  where
    tputScreenDimensions :: IO ScreenDimensions
    tputScreenDimensions =
      readProcess "tput" ["lines"] ""
      >>= \lines ->
        readProcess "tput" ["cols"] ""
        >>= \cols ->
              let lines' = read $ init lines
                  cols'  = read $ init cols
              in return $ ScreenDimensions lines' cols'

getTerminalSizeWithDefault :: IO ScreenDimensions
getTerminalSizeWithDefault =
  catch @IOException tputScreenDimensions $ \_e -> pure (ScreenDimensions 25 80)
  where
    tputScreenDimensions :: IO ScreenDimensions
    tputScreenDimensions =
      readProcess "tput" ["lines"] ""
      >>= \lines ->
        readProcess "tput" ["cols"] ""
        >>= \cols ->
              let lines' = read $ init lines
                  cols'  = read $ init cols
              in return $ ScreenDimensions lines' cols'

handleArgs :: IO (Either String FilePath)
handleArgs =
  parseArgs <$> Environment.getArgs
  where
    parseArgs argumentList =
      case argumentList of
        [fname] -> Right fname
        []      -> Left "no filename provided"
        _       -> Left "multiple files not supported"

eitherToErr :: Show a => Either a b -> IO b
eitherToErr (Right a) = return a
eitherToErr (Left e) =
  Exception.throwIO . IOError.userError $ show e

paginate :: ScreenDimensions -> FileInfo -> Text.Text -> [Text.Text]
paginate = undefined

showPages :: [Text.Text] -> IO ()
showPages _ = pure ()

fileInfo :: FilePath -> IO FileInfo
fileInfo _ = undefined

-- runHCat :: IO ()
-- runHCat = do
--   targetFilePath <- do
--     args <- handleArgs
--     eitherToErr args
--   contents <- do
--     handle <- openFile targetFilePath ReadMode
--     TextIO.hGetContents handle
--   hSetBuffering stdout NoBuffering
--   finfo <- fileInfo targetFilePath
--   termSize <- termSizeWithDefault finfo defaultScreenDimensions
--   let pages = paginate termSize finfo contents
--   showPages pages
--   where
--     defaultScreenDimensions = ScreenDimensions 25 80
--     showError finfo termSize err =
--       showPages $ paginate termSize finfo err
--     termSizeWithDefault finfo defaultTermSize = do
--       termSize <- getTerminalSizeEither
--       case termSize of
--         Left err -> do
--           showError finfo defaultTermSize (Text.pack err)
--           pure defaultTermSize
--         Right termSize' -> pure termSize'
--     getTerminalSizeEither =
--       catch @IOException (Right <$> tputScreenDimensions) $ \e -> pure $ Left (show e)
--     tputScreenDimensions =
--       readProcess "tput" ["lines"] ""
--       >>= \lines ->
--         readProcess "tput" ["cols"] ""
--         >>= \cols ->
--               let lines' = read $ init lines
--                   cols'  = read $ init cols
--               in return $ ScreenDimensions lines' cols'


-- runHCat :: IO ()
-- runHCat =
--   handleArgs
--   >>= eitherToErr
--   >>= flip openFile ReadMode
--   >>= TextIO.hGetContents
--   >>= \contents ->
--     getTerminalSize >>= \termSize ->
--       let pages = paginate termSize contents
--       in showPages pages
--   where
--     getTerminalSizeEither =
--       catch @IOException (Right <$> tputScreenDimensions) $ \e -> pure $ Left (show e)
--       where
--         tputScreenDimensions :: IO ScreenDimensions
--         tputScreenDimensions =
--           readProcess "tput" ["lines"] ""
--           >>= \lines ->
--             readProcess "tput" ["cols"] ""
--             >>= \cols ->
--                   let lines' = read $ init lines
--                       cols'  = read $ init cols
--                   in return $ ScreenDimensions lines' cols'

runHCat :: IO ()
runHCat = do
  targetFilePath <- do
    args <- handleArgs
    eitherToErr args
  contents <- do
    handle <- openFile targetFilePath ReadMode
    TextIO.hGetContents handle
  hSetBuffering stdout NoBuffering
  finfo <- fileInfo targetFilePath
  termSize <- termSizeWithDefault finfo defaultScreenDimensions
  let pages = paginate termSize finfo contents
  showPages pages
  where
    defaultScreenDimensions = ScreenDimensions 25 80

    showError finfo termSize err =
      showPages $ paginate termSize finfo err

    termSizeWithDefault finfo defaultTermSize = do
      termSize <- getTerminalSizeEither
      case termSize of
        Left err -> do
          showError finfo defaultTermSize (Text.pack err)
          pure defaultTermSize
        Right termSize' -> pure termSize'

    getTerminalSizeEither =
      catch @IOException tputScreenDimensions $ \e -> pure $ Left (show e)

    tput tputType =
      readProcess "tput" [tputType] ""

    tputEither tputType =
      catch @IOException (Right <$> tput tputType) $ \e -> pure $ Left (show e)

    nonEmptyStrStripNewline str
      | null str = Left "empty string"
      | str == "\n" = Left "empty string"
      | last str == '\n' = Right $ init str
      | otherwise = Left "missing newline"

    readTrailingNewline str =
      nonEmptyStrStripNewline str >>= readEither

    tputScreenDimensions = do
      termLines <- tputEither "lines"
      termCols <- tputEither "cols"
      pure $ do
        parsedLines <- readTrailingNewline =<< termLines
        parsedCols <- readTrailingNewline =<< termCols
        pure $ ScreenDimensions parsedLines parsedCols

unsnoc :: [a] -> Maybe ([a], a)
unsnoc [] = Nothing
unsnoc (x:xs) =
  case unsnoc xs of
    Nothing -> Just ([], x)
    Just (xs', x') -> Just (x:xs', x')

uncons :: [a] -> Maybe (a, [a])
uncons [] = Nothing
uncons (x:xs) = Just (x,xs)
