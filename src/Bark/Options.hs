{-# LANGUAGE DeriveDataTypeable, RecordWildCards #-}

module Bark.Options (
      Style(..)
    , Options(..)
    , parseOptions
    ) where

import Control.Monad      (when)
import Data.Maybe         (fromJust)
import Data.Version
import System.Console.CmdArgs
import System.Environment (getArgs, withArgs)
import System.Exit        (ExitCode(..), exitWith)
import Network.URI        (URI(..), parseURI)

data Style = Exact | Incremental deriving (Data, Typeable, Show, Eq)

data Options = Options
    { optDelimiter :: String
    , optService   :: String
    , optUri       :: URI
    , optBuffer    :: Int
    , optBound     :: Int
    , optParser    :: Style
    , optTee       :: Bool
    , optStrip     :: Bool
    } deriving (Data, Typeable, Show, Eq)

parseOptions :: IO Options
parseOptions = do
    argz <- getArgs
    opts <- (if null argz then withArgs ["--help"] else id) options
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
    exitWhen (null optService)     "--service cannot be blank"
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

    , optService = ""
        &= name "service"
        &= typ  "SERVICE"
        &= help "The application service name (required)"
        &= explicit

    , optUri = fromJust $ parseURI "amqp://guest:guest@127.0.0.1/"
        &= name "amqp-uri"
        &= typ  "URI"
        &= help "The amqp uri (default: guest@localhost)"
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

    , optParser = Exact
        &= name "parser"
        &= help "Use exact | incremental parsing (default: exact)"
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
