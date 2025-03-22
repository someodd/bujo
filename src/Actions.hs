{- | Manipulate the .bujo-data

-}

module Actions where

import Utils
import FileFormat
import qualified Data.Text as T

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