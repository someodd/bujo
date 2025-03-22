{- | This is merely an example!

-}

module Main (main) where

import Venusia.Server
import Routes
import qualified Data.ByteString.Char8 as BS

main :: IO ()
main = serve "7070" routes