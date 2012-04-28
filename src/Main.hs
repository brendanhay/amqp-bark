module Main (
  main
  ) where

import System.Environment (getArgs)
import Typhon.Options

main :: IO ()
main = do
    args <- getArgs
    print $ parse args

-- Read a line from stdin

-- Create routing information from env or start args

