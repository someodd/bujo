{- | Manage the bujo data directory.

# `.bujo-data` File standard

File looks like this:

    ---
    created: "2025-03-21"
    completed: false
    tags: [meeting, ideas]
    version: "0.1.0.0"
    ---
    This is the main text of the bullet journal entry. It contains your ideas, tasks, or notes.

    **Comments:**
    - First comment or additional note.
    - Another comment regarding the entry.

Directory structure looks like this:

    .bujo-data/
        2025/03/
            21/
            20/
            migrated/

-}

module FileFormat where

import Data.Yaml (decodeEither', encode)
import Data.Aeson (FromJSON, ToJSON)
import GHC.Generics (Generic)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
import qualified Data.Text as T
import qualified Data.Text.IO as T (readFile, writeFile)
import qualified Data.Text.Encoding as TE
import qualified Data.ByteString.Lazy as BL
import System.Directory (createDirectoryIfMissing, doesFileExist)
import System.FilePath ((</>), takeDirectory)

fileFormatVersion :: T.Text
fileFormatVersion = "0.1.0.0"

-- | The file extension for bullet journal entries
entryExtension :: String
entryExtension = ".bjentry"

dataDirectory :: FilePath
dataDirectory = ".bujo-data"

-- Make BujoHeader serializable to YAML
instance ToJSON BujoHeader

-- The YAML header, automatically decoded using generics.
data BujoHeader = BujoHeader
  { created :: T.Text
  , completed :: Bool
  , tags :: [T.Text]
  , version :: T.Text
  } deriving (Show, Generic)

instance FromJSON BujoHeader

-- The complete bullet journal entry consists of the header and the body text.
data BujoEntry = BujoEntry
  { header :: BujoHeader
  , body :: T.Text
  , comments :: [T.Text]
  } deriving (Show)

-- | Parse a bullet journal entry file.
-- It expects a YAML front matter block delimited by "---" on a line by itself.
parseBujoEntry :: T.Text -> Either T.Text BujoEntry
parseBujoEntry bs =
  let parts = T.splitOn "---\n" bs
  in case parts of
    (_ : yamlPart : bodyParts) ->
      case decodeEither' (TE.encodeUtf8 yamlPart) of
        Left err -> Left (T.pack $ show err)
        Right hdr -> 
          let fullBody = T.intercalate "\n---\n" bodyParts
              (mainBody, commentsList) = extractComments fullBody
          in Right $ BujoEntry hdr mainBody commentsList
    _ -> Left "Invalid format: missing YAML front matter"

-- Simple comment extraction - splits at "**Comments:**" and parses bullet points
extractComments :: T.Text -> (T.Text, [T.Text])
extractComments fullText =
  case T.splitOn "**Comments:**" fullText of
    [bodyOnly] -> (T.strip bodyOnly, [])
    [body, commentsSection] -> 
      let commentLines = T.lines commentsSection
          comments = [T.strip (T.drop 2 line) | line <- commentLines, T.isPrefixOf "- " (T.strip line)]
      in (T.strip body, comments)
    _ -> (fullText, []) -- Fallback for unexpected format

readBujoFile :: FilePath -> IO (Either T.Text BujoEntry)
readBujoFile filePath = do
  content <- T.readFile filePath
  case parseBujoEntry content of
    Left err -> pure $ Left ("Error parsing file: " <> err)
    Right entry -> pure $ Right entry

-- | Convert a BujoEntry to its text representation with YAML front matter
formatBujoEntry :: BujoEntry -> T.Text
formatBujoEntry entry =
  let headerYaml = TE.decodeUtf8 $ encode (header entry)
      commentSection = if null (comments entry)
                       then T.empty
                       else T.unlines $ 
                            ["**Comments:**"] ++ 
                            map (\c -> "- " <> c) (comments entry)
  in T.unlines
       [ "---"
       , headerYaml
       , "---"
       , body entry
       , if T.null commentSection then T.empty else "\n" <> commentSection
       ]

-- FIXME: the slug it generates sucks because of -- and replacing punctuation
-- | Create a filename for a BujoEntry based on its content
createBujoFilename :: BujoEntry -> T.Text
createBujoFilename entry = 
  let 
    -- Extract first few words from body for the name
    firstWords = T.take 25 $ T.takeWhile (/= '\n') (body entry)
    -- Remove problematic characters and replace spaces with underscores
    noPunctuation = T.filter (`notElem` ("\t\n\r!@#$%^&*()+=_-{}[]|\\:;\"'<>,.?/" :: String)) firstWords
    underscored = T.map (\c -> if c == ' ' then '_' else c) (T.unwords . T.words $ noPunctuation) -- this also removes >1 contiguous spaces with words functions
    -- Add timestamp from created field for uniqueness
    timestamp = T.filter (/= '-') (entry.header.created)
  in
    -- Combine sanitized title with timestamp
    underscored <> "-" <> timestamp

-- | Write a BujoEntry to the specified file path
--
-- Can effectively use this to "update" an entry, too.
writeBujoFile :: FilePath -> BujoEntry -> IO ()
writeBujoFile filePath entry = do
  -- Ensure the directory exists
  createDirectoryIfMissing True (takeDirectory filePath)
  print filePath
  print $ formatBujoEntry entry
  -- Format and write the entry
  T.writeFile filePath (formatBujoEntry entry)

-- | Creates the standard path for a bujo entry based on creation date
-- Example: .bujo-data/2025/03/21/entry-name.bujo
createBujoPath :: T.Text -> T.Text -> FilePath
createBujoPath date entryName =
  let [year, month, day] = map T.unpack $ T.splitOn "-" date
  in dataDirectory </> year </> month </> day </> T.unpack entryName <> entryExtension

-- | Write a BujoEntry to the standard location based on its creation date
writeNewBujoFile :: BujoEntry -> IO (Either T.Text FilePath)
writeNewBujoFile entry = do
  let filePath = createBujoPath entry.header.created (createBujoFilename entry)
  exists <- doesFileExist filePath
  if exists
    then do
      hopefullyExistingEntry <- readBujoFile filePath
      let existingEntry = formatBujoEntry <$> hopefullyExistingEntry
      pure . Left $ "Tried adding, but file already exists: " <> (T.pack filePath) <> "\r\n" <> either id id existingEntry
    else do
      writeBujoFile filePath entry
      pure $ Right filePath