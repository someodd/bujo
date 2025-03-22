module Routes where

import Actions
import FileFormat
import Venusia.Server
import Venusia.MenuBuilder
import qualified Data.Text as T
import Prelude hiding (error)

{- | The default root menu which displays the default view of the bullet journal.

This is like the default view, there are other views, like
viewing the menu which makes all the entries tap to delete.

-}
routeRootIndexDefault :: Request -> IO T.Text
routeRootIndexDefault _ = do
    hopefullyEntry <- readBujoFile ".bujo-data/demo.bujo"
    let
        entryToMenu entry =
            [ info . T.pack . show $ entry.header
            , info . T.pack . show $ entry.body
            , info . T.pack . show $ entry.comments
            , search "Add entry" "/add" "localhost" 7070
            ]
        entryResults = either ((:[]) . info) entryToMenu hopefullyEntry

    pure . render $ entryResults

-- FIXME: confirm overwrite?
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

{- | ALL of the routes for the Venusia server.

-}
routes :: [Route]
routes =
    [ on "" routeRootIndexDefault
    , on "/add" routeAddEntry
    ]