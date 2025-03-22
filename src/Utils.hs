module Utils where

import Data.Time (getCurrentTime, utctDay, UTCTime, toGregorian)
import Data.Time.Format (formatTime, defaultTimeLocale)
import Data.Time.LocalTime (getCurrentTimeZone, utcToLocalTime)
import qualified Data.Text as T
import System.Directory (listDirectory, doesDirectoryExist)
import System.FilePath ((</>))

-- | Get the current date in YYYY-MM-DD format using local time
getCurrentDateText :: IO T.Text
getCurrentDateText = do
    now <- getCurrentTime
    tz <- getCurrentTimeZone
    let localTime = utcToLocalTime tz now
    return $ T.pack $ formatTime defaultTimeLocale "%Y-%m-%d" localTime

-- | Get the current date components (year, month, day) as Text using local time
getCurrentDateComponents :: IO (T.Text, T.Text, T.Text)
getCurrentDateComponents = do
    now <- getCurrentTime
    tz <- getCurrentTimeZone
    let localTime = utcToLocalTime tz now
        yearText = T.pack $ formatTime defaultTimeLocale "%Y" localTime
        -- Format month and day with leading zeros
        monthText = T.pack $ formatTime defaultTimeLocale "%m" localTime
        dayText = T.pack $ formatTime defaultTimeLocale "%d" localTime
    return (yearText, monthText, dayText)

{- | The current date (from path) is not the current month.

-}
isNotThisMonth :: FilePath -> IO Bool
isNotThisMonth path = do
    (year, month, _) <- getCurrentDateComponents
    let pathParts = T.splitOn "/" (T.pack path)
        -- Assuming the path is structured as ".bujo-data/YYYY/MM/.../entry.bjentry"
        entryYear = head pathParts
        entryMonth = pathParts !! 1
    return $ entryYear /= year || entryMonth /= month

-- | Recursively list all files in a directory
listDirectoryRecursive :: FilePath -> IO [FilePath]
listDirectoryRecursive dir = do
    entries <- listDirectory dir
    paths <- mapM processEntry entries
    return (concat paths)
  where
    processEntry entry = do
        let fullPath = dir </> entry
        isDir <- doesDirectoryExist fullPath
        if isDir
            then listDirectoryRecursive fullPath
            else return [fullPath]