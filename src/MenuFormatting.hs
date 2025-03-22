{- | Things to help with rendering.

-}

module MenuFormatting where

import FileFormat
import Venusia.MenuBuilder
import qualified Data.Text as T
import Prelude hiding (error)
import System.FilePath ((</>))
import Data.Maybe (fromMaybe)
import Data.List (stripPrefix)
import Data.List (sortOn)
import Data.Ord (Down(..))


{- | A cute little way of displaying tags for a post as text.

-}
tagText :: [T.Text] -> T.Text
tagText [] = ""
tagText tags =
   ("[" <>) . (<> "]") . T.dropWhile (==' ') $ T.intercalate "|" $ T.strip <$> tags

{- | Take a list of file paths associated with a bujo entry, a function to turn them into
Text lines, and group them by date, separating the dates with an element/line
displaying the date.

-}
displayEntriesWithDateHeaders :: (FilePath -> BujoEntry -> T.Text -> T.Text) -> [(FilePath, BujoEntry)] -> [T.Text]
displayEntriesWithDateHeaders lineTransformer entries =
    insertHeaders $ map formatEntry $ sortOn (Down . getDate . snd) entries
  where
    getDate :: BujoEntry -> T.Text
    getDate entry = entry.header.created
    
    formatEntry :: (FilePath, BujoEntry) -> (T.Text, T.Text)
    formatEntry (path, entry) =
      (getDate entry, lineTransformer path entry (T.strip entry.body <> " " <> tagText entry.header.tags))
    
    insertHeaders :: [(T.Text, T.Text)] -> [T.Text]
    insertHeaders [] = []
    insertHeaders ((date, text):xs) =
      dateHeader date : text : insertHeaders' date xs
    
    insertHeaders' :: T.Text -> [(T.Text, T.Text)] -> [T.Text]
    insertHeaders' _ [] = []
    insertHeaders' prevDate ((date, text):xs)
      | date == prevDate = text : insertHeaders' date xs
      | otherwise = dateHeader date : text : insertHeaders' date xs
    
    dateHeader :: T.Text -> T.Text
    dateHeader day = info $ rulerRight '.' day


bujoData :: FilePath
bujoData = ".bujo-data/"

{- | Gives a more selector-friendly version of a path by simply removing the

-}
stripPathBujoData :: FilePath -> FilePath
stripPathBujoData path =
    fromMaybe path (stripPrefix bujoData path)

{- | Transform a (FilePath, BujoEntry) into a menu item.
-}
entryToMenuItem :: FilePath -> (FilePath, BujoEntry) -> T.Text
entryToMenuItem selectorPrefix (path, entry) =
  let
    -- Remove .bujo-data/ prefix from the path
    cleanPath = stripPathBujoData path
    -- Format the entry with the header and body
    formattedEntry = formatBujoEntry entry
    -- Create a link to view the entry
    viewLink = directory (T.take width . T.strip $ entry.body) (T.pack $ selectorPrefix </> cleanPath) "localhost" 7070
  in
    T.unwords [viewLink, formattedEntry]

-- | A nice suggestion for how wide to allow text to be in a menu.
width :: Int
width = 30

-- | Used for decoration, separating sections.
separator :: T.Text
separator = T.replicate width "-"

-- | Adds a space and then a bunch of = signs until the line is the right widht.
rulerRight :: Char-> T.Text -> T.Text
rulerRight rulerChar text =
    let
        textLength = T.length text
        padding = width - textLength - 1
        paddingText = T.replicate padding $ T.pack [rulerChar]
    in
        T.concat [text, " ", paddingText]

-- would this be good venusia.menubuilder?
-- | A little helper function to add consistent formatting to the menu.
menuFrame header body =
    let
        headerText = info $ "bujo: " <> header
        indexLink = directory "Root menu" "" "localhost" 7070
        tagsLink = directory "View tags" "/tags" "localhost" 7070
        addEntryLink = search "Add entry" "/add" "localhost" 7070
    in
        [headerText, info separator] ++
        body ++
        [info separator, tagsLink, indexLink, "", addEntryLink]