module Utils where

import Data.Time (getCurrentTime, utctDay)
import Data.Time.Format (formatTime, defaultTimeLocale)
import qualified Data.Text as T

-- | Get the current date in YYYY-MM-DD format
getCurrentDateText :: IO T.Text
getCurrentDateText = do
  now <- getCurrentTime
  let today = utctDay now
  return $ T.pack $ formatTime defaultTimeLocale "%Y-%m-%d" today