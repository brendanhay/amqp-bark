{-# LANGUAGE DeriveDataTypeable, RecordWildCards #-}

module Bark.Options (
      Options(..)
    , parseOptions
    ) where

import Control.Monad                 (when)
import Data.Maybe                    (fromJust)
import Data.Version
import System.Console.CmdArgs hiding (args)
import System.Environment            (getArgs, withArgs)
import System.Exit                   (ExitCode(..), exitWith)
import Network.URI                   (URI(..), parseURI)

data Options = Options
    { optDelimiter :: String
    , optName      :: String
    , optUri       :: URI
    , optBuffer    :: Int
    , optBound     :: Int
    , optTee       :: Bool
    , optStrip     :: Bool
    } deriving (Data, Typeable, Show, Eq)

parseOptions :: IO Options
parseOptions = do
    args <- getArgs
    opts <- (if null args then withArgs ["--help"] else id) options
    validate opts

options :: IO Options
options = cmdArgs $ defaults
    &= versionArg [explicit, name "version", name "v", vers]
    &= summary blank
    &= helpArg [explicit, name "help", name "h"]
    &= program usage
  where
    blank = ""
    usage = "Usage: " ++ application
    vers  = summary $ concat [application, ": ", showVersion version]

application :: String
application = "amqp-bark"

version :: Version
version = Version
    { versionBranch = [0, 1, 0]
    , versionTags   = ["experimental"]
    }

validate :: Options -> IO Options
validate opts@Options{..} = do
    exitWhen (null optDelimiter)   "--delimiter cannot be blank"
    exitWhen (null optName)        "--name cannot be blank"
    exitWhen (not $ optBuffer > 0) "--buffer must be greater than zero"
    exitWhen (not $ optBound > 0)  "--bound must be greater than zero"

    return opts

exitWhen :: Bool -> String -> IO ()
exitWhen p msg = when p $ putStrLn msg >> exitWith (ExitFailure 1)

defaults :: Options
defaults = Options
    { optDelimiter = "\n"
        &= name "delimiter"
        &= typ  "STRING"
        &= help "A byte or string denoting output (default: \\n)"
        &= explicit

    , optName = ""
        &= name "name"
        &= typ  "NAME"
        &= help "The application name (required)"
        &= explicit

    , optBuffer = 4096
        &= name "buffer"
        &= typ  "BYTES"
        &= help "The size of the stdin buffer (default: 4096)"
        &= explicit

    , optBound = 512
        &= name "bound"
        &= typ  "INT"
        &= help "The max number of '--buffer' chunks (default: 512)"
        &= explicit

    , optUri = fromJust $ parseURI "amqp://guest:guest@127.0.0.1/"
        &= name "amqp-uri"
        &= typ  "URI"
        &= help "The amqp uri (default: guest@localhost)"
        &= explicit

    , optStrip = False
        &= name "strip"
        &= help "Remove the delimiter from output (default: false)"
        &= explicit

    , optTee = False
        &= name "tee"
        &= help "Additionally write output to stdout (default: false)"
        &= explicit
    }
