{-# LANGUAGE DeriveDataTypeable #-}

module Main (
      main
    ) where

import System.Console.CmdArgs
import System.Exit        (ExitCode(..), exitWith)

data Options = Options
    { optName :: String
    , optType :: String
    } deriving (Show, Data, Typeable)

options = Options
     { optName = def &= explicit &= name "name" &= help "Application name" &= typ "NAME"
     , optType = def &= explicit &= name "type" &= help "Application type" &= typ "TYPE"
     } &= summary ("Typhon " ++ version)

main :: IO ()
main = do
    parsed <- cmdArgs options
    print $ valid parsed

version :: String
version = "0.1"

valid :: Options -> Options
valid Options { optName = [] } = error "name cannot be blank"
valid Options { optType = [] } = error "type cannot be blank"
valid opts                     = opts
