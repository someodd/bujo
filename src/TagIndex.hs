{- | Tools for maintaining the tag index.
   Can regenerate index.
-}
module TagIndex where

import Utils
import FileFormat
import qualified Data.Text as T
import qualified Data.Text.IO as T (readFile)
import Data.List (isSuffixOf, nub)
import Control.Monad (foldM)

{- | Get every .bujo file in the .bujo-data directory and extract their tags.
-}
getAllTags :: IO [T.Text]
getAllTags = do
  files <- listDirectoryRecursive dataDirectory
  let bujoFiles = filter (isSuffixOf entryExtension) files
  nub <$> foldM (\acc file -> do
          tags <- extractTagsFromFile file
          return (acc ++ tags)) [] bujoFiles

-- | Extract tags from a single file
extractTagsFromFile :: FilePath -> IO [T.Text]
extractTagsFromFile file = do
  content <- T.readFile file
  case parseBujoEntry content of
    Left _ -> do
        return [] -- Ignore files that can't be parsed
    Right entry -> do
        return entry.header.tags