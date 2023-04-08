{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}

import Control.Monad
import Data.List
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Monoid (mappend, mconcat)
import Data.String
import Hakyll
import Text.Pandoc.Highlighting (
  Style,
  breezeDark,
  styleToCss,
 )
import Text.Pandoc.Options (
  ReaderOptions (..),
  WriterOptions (..),
 )
import Text.Pandoc.Definition

pandocCodeStyle :: Style
pandocCodeStyle = breezeDark

pandocCompiler' :: Compiler (Item String)
pandocCompiler' =
  pandocCompilerWithTransform defaultHakyllReaderOptions writer customPandoc
  where
    writer =
      defaultHakyllWriterOptions {writerHighlightStyle = Just pandocCodeStyle}

--------------------------------------------------------------------------------
main :: IO ()
main = hakyll $ do
  match "images/*" $ do
    route idRoute
    compile copyFileCompiler

  match "css/*" $ do
    route idRoute
    compile compressCssCompiler

  match (fromList ["favicon.png"]) $ do
    route idRoute
    compile copyFileCompiler

  match "solutions/*/*" $ do
    route $ setExtension "html"
    compile $ do
      let
        titleField = field "title" lookupSolutionName
        ctx = titleField <> defaultContext
      pandocCompiler'
        >>= saveSnapshot "solutionPreTemplate"
        >>= loadAndApplyTemplate "templates/exercise-solution.html" ctx
        >>= saveSnapshot "solutionPostExerciseTemplate"
        >>= loadAndApplyTemplate "templates/default.html" ctx
        >>= relativizeUrls

  match "solutions/solutions.md" $ do
    route $ setExtension "html"
    compile $ do
      solutions <- sortExercises =<< loadAll "solutions/*/*"
      let
        solutionList = listField "solutions" defaultContext (pure solutions)
        ctx = defaultContext <> solutionList
      getResourceBody
        >>= applyAsTemplate ctx
        >>= loadAndApplyTemplate "templates/default.html" ctx
        >>= relativizeUrls

  match "chapters/*.md" $ do
    route $ setExtension "html"
    compile $ do
      chapter <- (`getMetadataField'` "chapter") =<<  getUnderlying
      rawSolutions <- loadAllSnapshots "solutions/*/*" "solutionPreTemplate"
      solutions <- exercisesForChapter chapter rawSolutions

      let
        solutionCtx =
          listField "solutions" defaultContext $ pure solutions
        titleField = field "title" lookupChapterTitle
        ctx = titleField <> defaultContext <> solutionCtx <> titleField

      pandocCompiler'
        >>= saveSnapshot "chaptersPreTemplate"
        >>= applyAsTemplate ctx
        >>= loadAndApplyTemplate "templates/chapter-template.html" ctx
        >>= loadAndApplyTemplate "templates/default.html" ctx
        >>= relativizeUrls

  create ["chapters/table-of-contents.html"] $ do
    route idRoute
    compile $ do
      chapters <- do
        chpList <- loadAllSnapshots "chapters/*.md" "chaptersPreTemplate"
        sortChapters chpList
      let
        f = field "title" lookupChapterTitle
        chpCtx =
          listField "chapters" (f <> defaultContext) $ pure chapters
        contentsTitle = constField "title" "Effective Haskell: Table of Contents"
        ctx = contentsTitle <> defaultContext <> chpCtx
      makeItem ""
        >>= loadAndApplyTemplate "templates/chapter-list.html" ctx
        >>= loadAndApplyTemplate "templates/default.html" ctx
        >>= relativizeUrls

  match "projects/*" $ do
    route $ setExtension "html"
    compile $
      pandocCompiler'
        >>= saveSnapshot "postPreTemplate"
        >>= loadAndApplyTemplate "templates/post.html" postCtx
        >>= loadAndApplyTemplate "templates/default.html" postCtx
        >>= relativizeUrls

  -- create ["archive.html"] $ do
  --   route idRoute
  --   compile $ do
  --     posts <- recentFirst =<< loadAll "posts/*"
  --     let archiveCtx =
  --           listField "posts" postCtx (return posts)
  --             `mappend` constField "title" "Archives"
  --             `mappend` defaultContext

  --     makeItem ""
  --       >>= loadAndApplyTemplate "templates/archive.html" archiveCtx
  --       >>= loadAndApplyTemplate "templates/default.html" archiveCtx
  --       >>= relativizeUrls

  match "setup/getting-started.md" $ do
    route $ setExtension "html"
    compile $
      pandocCompiler'
      >>= loadAndApplyTemplate "templates/default.html" defaultContext
      >>= relativizeUrls

  match "index.html" $ do
    route idRoute
    compile $ do
      --      solutions <- loadAll
      -- posts <- recentFirst =<< loadAll "solutions/*/*"

      -- let
      --   indexCtx =
      --     mconcat
      --       [ listField "posts" postCtx (return posts)
      --       , defaultContext
      --       ]

      getResourceBody
        >>= applyAsTemplate defaultContext
        >>= loadAndApplyTemplate "templates/default.html" defaultContext
        >>= relativizeUrls

  match "templates/*" $ compile templateBodyCompiler

--------------------------------------------------------------------------------

{-# NOINLINE chapterNames #-}
chapterNames :: Map String Identifier
chapterNames =
  Map.fromList $
    [ ("1", "Getting Started with Haskell")
    , ("2", "Working with Lists")
    , ("3", "Getting Started with Types")
    , ("4", "Creating New Types")
    , ("5", "Creating and Structuring Haskell Projects")
    , ("6", "Type Classes")
    , ("7", "Understanding IO")
    , ("8", "Working with the Local System")
    , ("9", "Introducing Monads")
    , ("10", "Mutable Data in the Real World")
    , ("11", "Serializing Heterogenous Data")
    , ("12", "Deserializing Heterogenous Data")
    , ("13", "Building Applications with Many Effects")
    , ("14", "Building Efficient Programs")
    , ("15", "Programming with Types")
    ]

lookupChapterTitle :: Item a -> Compiler String
lookupChapterTitle item = do
  chapterId <- getMetadataField' (itemIdentifier item) "chapter"
  pure . toFilePath $ chapterNames Map.! chapterId

lookupSolutionName :: Item a -> Compiler String
lookupSolutionName item = getMetadataField' (itemIdentifier item) "name"

sortExercises :: (MonadMetadata m, MonadFail m) => [Item a] -> m [Item a]
sortExercises items =
  map snd . sortOn fst <$> traverse itemWithOrd items
  where
    itemWithOrd item = (,item) <$> chapterOrd item
    chapterOrd item = (,) <$> getChapter item <*> getExercise item
    getChapter item =
      read @Int <$> getMetadataField' (itemIdentifier item) "chapter"
    getExercise item =
      read @Int <$> getMetadataField' (itemIdentifier item) "exercise-id"

sortChapters :: (MonadMetadata m, MonadFail m) => [Item a] -> m [Item a]
sortChapters items = map snd . sortOn (read @Int . fst) <$> traverse withChapter items
  where
    withChapter item =
      (,item) <$> getMetadataField' (itemIdentifier item) "chapter"

exercisesForChapter ::
  String ->  [Item a] -> Compiler [Item a]
exercisesForChapter chapterID solutions =
  filterM solutionForChapter solutions >>= sortExercises
  where
    solutionForChapter solution = do
      solutionChapter <- getMetadataField' (itemIdentifier solution) "chapter"
      pure $ chapterID == solutionChapter

customPandoc :: Pandoc -> Pandoc
customPandoc = id
