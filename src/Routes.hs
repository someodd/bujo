module Routes where

import Utils
import MenuFormatting
import TagIndex
import Actions
import FileFormat
import Venusia.Server
import Venusia.MenuBuilder
import qualified Data.Text as T
import Prelude hiding (error)
import System.FilePath ((</>))
import qualified Data.Set as Set

hostname :: T.Text
hostname = "localhost"

port :: Int
port = 7070

selectorEntries :: T.Text
selectorEntries = "/entries"

selectorCompleteEntry :: T.Text
selectorCompleteEntry = "/complete"

selectorUncompleteEntry :: T.Text
selectorUncompleteEntry = "/uncomplete"


{- | Menu items for a given entry, right now specifically made for root.

-}
entryMenu :: FilePath -> BujoEntry -> T.Text -> IO [T.Text]
entryMenu pathRaw entry displayString = do
    migrateAvailable <- isNotThisMonth pathRaw
    pure $
        [ info displayString
        , directory "View" (mkPath "/entries/") hostname port
        , if entry.header.completed then directory "Mark incomplete" (mkPath "/incomplete/") hostname port else directory "Mark complete" (mkPath "/complete/") hostname port
        , directory "Delete" (mkPath "/delete/") hostname port
        , search "Edit" (mkPath "/edit/") hostname port
        ] ++
        [ directory "Migrate" (mkPath "/migrate/") hostname port | migrateAvailable ]
  where
    mkPath prefix = T.pack $ prefix </> stripPathBujoData pathRaw

{- | The default root menu which displays the default view of the bullet journal.

This is like the default view, there are other views, like
viewing the menu which makes all the entries tap to delete.

-}
routeRootIndexDefault :: Request -> IO T.Text
routeRootIndexDefault _ = do
  thisMonthEntries <- entriesThisMonth
  notThisMonthEntries <- entriesNotThisMonth
  allTags <- getAllTags
  migratedEntries' <- migratedEntries
  let migratedEntries'' = entryToMenuItem ("/entry/") <$> migratedEntries'
  
  let
    -- Process this month's entries, tracking seen dates
    monthEntriesTexts = displayEntriesWithDateHeaders 
        (\path _ text -> directory text (T.pack $ (T.unpack selectorEntries) </> stripPathBujoData path) hostname port)
        thisMonthEntries
    -- Create the formatted lines
    otherEntries = entryToMenuItem (T.unpack selectorEntries) <$> reverse notThisMonthEntries

    tagLink tag = directory tag ("/tags/" <> tag) hostname port
    
  pure . render . menuFrame "Root menu" $
    [ info $ rulerRight '=' "Tags:" ] ++
    (tagLink <$> allTags) ++
    [info $ rulerRight '=' "Entries this month:"] ++
    monthEntriesTexts ++
    [ info $ rulerRight '-' "Migrated entries:" ] ++ migratedEntries'' ++
    [ info $ rulerRight '=' "Entries not this month:" ] ++
    otherEntries

{- | Route for adding a new entry.

-}
routeAddEntry :: Request -> IO T.Text
routeAddEntry request =
    case request.reqQuery of
        Just query -> do
            hopefullyPathAndEntry <- actionCreateEntry query
            case hopefullyPathAndEntry of
                Left someError -> do
                    pure . render $ info <$> T.lines someError
                Right (path, entry) ->
                    let entryAsMenuText = info <$> (T.lines $ formatBujoEntry entry)
                    in pure . render $ [info $ "Wrote file: " <> T.pack path] ++ entryAsMenuText
        Nothing ->
            pure $ render [error "Trying to create a new entry, yet no text provided (blank query)."]

{- | Simply view all tags (route) for the current month.

-}
routeViewTags :: Request -> IO T.Text
routeViewTags _ = do
    tags <- getAllTags
    pure . render $ info <$> tags

viewEntry :: FilePath -> T.Text -> [T.Text] -> IO T.Text
viewEntry fullPath frameName headerLines = do
    maybeEntry <- readBujoFile fullPath
    case maybeEntry of
        Left someError -> do
            pure . render $ [error $ "Error reading file: " <> someError]
        Right entry -> do
            entryMenuLines <- entryMenu fullPath entry "Link/Reload"
            let entryAsMenuText = (info <$> (T.lines $ formatBujoEntry entry)) ++ entryMenuLines
            pure . render $ menuFrame "Viewing entry" $ headerLines ++ entryAsMenuText

selectorToFullPath :: T.Text -> FilePath
selectorToFullPath selectorPath =
    bujoData </> (T.unpack selectorPath)

{- | View an entry (route).

-}
routeViewEntry :: Request -> IO T.Text
routeViewEntry request =
    case request.reqWildcard of
        Just path ->
            viewEntry (selectorToFullPath path) path []
        Nothing ->
            pure $ render [error "Trying to view an entry, yet no path provided (blank path)."]

{- | Route to view the current month's entries with the specified tag.

-}
routeViewThisMonthsEntriesWithTag :: Request -> IO T.Text
routeViewThisMonthsEntriesWithTag request =
    case request.reqWildcard of
        Just tag -> do
            thisMonthEntries <- entriesThisMonth
            let filteredEntries = filter (entryHasTag tag) thisMonthEntries
                entryTexts = entryToMenuItem "/entries" <$> filteredEntries
                body = [info $ rulerRight '=' $ "Entries with tag: " <> tag] ++ entryTexts
            pure . render $ menuFrame "Entries with Tag" body
        Nothing ->
            pure $ render [error "Trying to view entries with a tag, yet no tag provided (blank wildcard)."]
  where
    entryHasTag :: T.Text -> (FilePath, BujoEntry) -> Bool
    entryHasTag tag (_, entry) = tag `elem` entry.header.tags

{- | Route to delete an entry.

-}
routeDeleteEntry :: Request -> IO T.Text
routeDeleteEntry request =
    case request.reqWildcard of
        Just path -> do
            deleted <- actionDeleteEntry (T.unpack path)
            if deleted
                then pure . render . menuFrame "Successful deletion" $ [info $ "Deleted entry: " <> path]
                else pure . render . menuFrame "Deletion failed" $ [error $ "Entry not found: " <> path]
        Nothing ->
            pure $ render . menuFrame "Deletion failed" $ [error "Trying to delete an entry, yet no path provided (blank wildcard)."]

{- | Route to mark an entry as complete.
-}
routeCompleteEntry :: Request -> IO T.Text
routeCompleteEntry request =
    case request.reqWildcard of
        Just selectorPath -> do
            let pathOnDisk = selectorToFullPath selectorPath
            print pathOnDisk
            completed <- completeEntry (pathOnDisk) True
            case completed of
                Left err -> pure . render $ [error err]
                Right _ ->
                    viewEntry pathOnDisk "Marked complete" $ [info $ "Marked as complete: " <> T.pack pathOnDisk]
        Nothing ->
            pure $ render . menuFrame "Error trying to mark as complete" $ [error "Trying to mark an entry as complete, yet no path provided (blank wildcard)."]

{- | Route to mark an entry as incomplete.
-}
routeIncompleteEntry :: Request -> IO T.Text
routeIncompleteEntry request =
    case request.reqWildcard of
        Just selectorPath -> do
            let pathOnDisk = selectorToFullPath selectorPath
            print pathOnDisk
            completed <- completeEntry pathOnDisk False
            case completed of
                Left err -> pure . render $ [error err]
                Right _ ->
                    viewEntry pathOnDisk "Marked complete" $  [info $ "Marked as incomplete: " <> T.pack pathOnDisk]
        Nothing ->
            pure $ render . menuFrame "Error trying to mark as incomplete" $ [error "Trying to mark an entry as incomplete, yet no path provided (blank wildcard)."]

{- | Route to migrate an entry.
-}
routeMigrateEntry :: Request -> IO T.Text
routeMigrateEntry request =
    case request.reqWildcard of
        Just selectorPath -> do
            let pathOnDisk = selectorToFullPath selectorPath
            migrateAvailable <- isNotThisMonth pathOnDisk
            if migrateAvailable
                then do
                    -- Perform migration logic here
                    newPathOnDisk <- migrateEntry pathOnDisk
                    pure . render $ [info $ "Migrated entry " <> T.pack pathOnDisk <> " to " <> T.pack newPathOnDisk]
                else pure . render $ [error $ "Entry not available for migration: " <> T.pack pathOnDisk]
        Nothing ->
            pure $ render . menuFrame "Migration failed" $ [error "Trying to migrate an entry, yet no path provided (blank wildcard)."]

{- | ALL of the routes for the Venusia server.

-}
routes :: [Route]
routes =
    [ on "/" routeRootIndexDefault
    , on "" routeRootIndexDefault
    , on "/add" routeAddEntry
    , onWildcard "/tags/*" routeViewThisMonthsEntriesWithTag
    , onWildcard "/delete/*" routeDeleteEntry
    , on "/tags" routeViewTags
    , onWildcard "/entries/*" routeViewEntry
    , onWildcard "/complete/*" routeCompleteEntry
    , onWildcard "/incomplete/*" routeIncompleteEntry
    , onWildcard "/migrate/*" routeMigrateEntry
    ]