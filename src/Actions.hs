{- | Manipulate the .bujo-data

Should move shit to querying the flat files like a DB for some stuff.

-}

module Actions where

import Utils
import FileFormat
import qualified Data.Text as T
import Data.List (isSuffixOf, isPrefixOf)
import System.FilePath ((</>), takeFileName, takeDirectory)
import System.Directory (doesFileExist, doesDirectoryExist, createDirectoryIfMissing, renameFile)
import System.Directory (removeFile)
import Data.Functor ((<&>))

migrateDirectoryName = "migrate"

{- | Migrate an entry simply by moving it to the current month/date according to the format/directory structure we expect.

Returns the file path to the location it was moved to.

Currently does not handle errors.

-}
migrateEntry :: FilePath -> IO FilePath
migrateEntry pathOnDisk = do
    (year, month, day) <- getCurrentDateComponents
    let newPath = dataDirectory </> T.unpack year </> T.unpack month </> migrateDirectoryName </> takeFileName pathOnDisk
    createDirectoryIfMissing True (takeDirectory newPath)
    renameFile pathOnDisk newPath
    pure newPath

{- | Get list of all entries in the current month's migration directory.

-}
migratedEntries :: IO [(FilePath, BujoEntry)]
migratedEntries = do
    (year, month, _) <- getCurrentDateComponents
    let path = dataDirectory </> T.unpack year </> T.unpack month </> migrateDirectoryName
    entries <- readBujoFilesInDirectory path (const True)
    pure entries

-- | Parses a piece of text into an Entry, so you can tagg entries and the like easily using shorthand syntax.
--
-- Returns the actual body text and then the tags.
entryCommandParser :: T.Text -> (T.Text, [T.Text])
entryCommandParser command =
    case T.splitOn "::" command of
        [left, right] -> (T.strip right, T.words left)
        _ -> (command, [])

-- fail if already exists?
-- can actually get creative like last time with one line format tags all that
actionCreateEntry :: T.Text -> IO (Either T.Text (FilePath, BujoEntry))
actionCreateEntry createText = do
    created <- getCurrentDateText
    let
        (body, tags) = entryCommandParser createText
        header = BujoHeader
            { created = created
            , version = fileFormatVersion
            , completed = False
            , tags = tags
            }
        entry = BujoEntry
            { header = header
            , body = body
            , comments = []
            }
    hopefullyEntry <- writeNewBujoFile entry
    pure $ (,entry) <$> hopefullyEntry

{- | Return all entries from this month.

-}
entriesThisMonth :: IO [(FilePath, BujoEntry)]
entriesThisMonth = do
    (year, month, _) <- getCurrentDateComponents
    let path = dataDirectory </> T.unpack year </> T.unpack month
    readBujoFilesInDirectory path (const True)

{- | Delete a specific entry.

Returns True if the entry was deleted, False if it didn't exist.

-}
actionDeleteEntry :: FilePath -> IO Bool
actionDeleteEntry path = do
    let fullPath = dataDirectory </> path
    exists <- doesFileExist fullPath
    if exists
        then do
            removeFile fullPath
            pure True
        else pure False

{- | Return all entries NOT from this month.

-}
entriesNotThisMonth :: IO [(FilePath, BujoEntry)]
entriesNotThisMonth = do
    (year, month, _) <- getCurrentDateComponents
    let path = dataDirectory
    readBujoFilesInDirectory path (\x -> not $ (dataDirectory </> T.unpack year </> T.unpack month) `isPrefixOf` x)

{- | Set an entry as "completed".

-}
completeEntry :: FilePath -> Bool -> IO (Either T.Text (FilePath, BujoEntry))
completeEntry fullPath completed = do
    exists <- doesFileExist fullPath
    if exists
        then do
            hopefullyEntry <- readBujoFile fullPath
            case hopefullyEntry of
                Left err -> pure $ Left err
                Right entry -> do
                    let updatedEntry = entry { header = (header entry) { completed = completed } }
                    writeBujoFile fullPath updatedEntry
                    pure $ Right (fullPath, updatedEntry)
        else pure $ Left "File does not exist."

{- | All entries tagged with a specific tag.

-}
entriesWithTagCurrentMonth :: T.Text -> IO [(FilePath, BujoEntry)]
entriesWithTagCurrentMonth tag = do
    (year, month, _) <- getCurrentDateComponents
    let path = dataDirectory </> T.unpack year </> T.unpack month
    allEntries <- readBujoFilesInDirectory path (const True)
    let taggedEntries = filter (\(_, entry) -> tag `elem` entry.header.tags) allEntries
    pure taggedEntries

readBujoFilesInDirectory :: FilePath -> (FilePath -> Bool) -> IO [(FilePath, BujoEntry)]
readBujoFilesInDirectory dirPath filterCriteria = do
    dirExists <- doesDirectoryExist dirPath
    if not dirExists
        then pure []
        else do
            files <- listDirectoryRecursive dirPath
            let bujoFiles = filter filterCriteria . filter (isSuffixOf entryExtension) $ files
            entries <- mapM (\x -> readBujoFile x <&> (x,)) bujoFiles
            let successfulEntries = [(filePath, entry) | (filePath, Right entry) <- entries]
            pure successfulEntries