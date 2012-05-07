{-# LANGUAGE DeriveDataTypeable, RecordWildCards #-}

module Bark.Options (
      Options
    , parseOptions
    ) where

import Data.Maybe         (fromJust)
import Data.Version
import System.Console.CmdArgs
import System.Environment (getArgs, withArgs)
import System.Exit
import Network.URI        (URI(..), parseURI)

import qualified Control.Monad as M

import qualified Data.ByteString as BS

data Options = Options
    { optDelimiter :: String
    , optStrip     :: Bool
    , optBuffer    :: Int
    , optBound     :: Int
    , optTee       :: Bool
    , optUri       :: URI
    , optName      :: String
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
    when (null optDelimiter)   "--delimiter cannot be blank"
    when (not $ optBuffer > 0) "--buffer must be greater than zero"
    when (not $ optBound > 0)  "--bound must be greater than zero"
    when (null optName)        "--name cannot be blank"
    return opts

when :: Bool -> String -> IO ()
when p msg = M.when p $ putStrLn msg >> exitWith (ExitFailure 1)

defaults :: Options
defaults = Options
    { optDelimiter = "\n"
        &= name "delimiter"
        &= typ  "STRING"
        &= help "A byte or string denoting output (default: \\n)"
        &= explicit

    , optStrip = False
        &= name "strip"
        &= help "Remove the delimiter from output (default: false)"
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

    , optTee = False
        &= name "tee"
        &= help "Additionally write output to stdout (default: false)"
        &= explicit

    , optUri = fromJust $ parseURI "amqp://guest:guest@127.0.0.1/"
        &= name "amqp-uri"
        &= typ  "URI"
        &= help "The amqp uri (default: guest@localhost)"
        &= explicit

    , optName = ""
        &= name "name"
        &= typ  "NAME"
        &= help "The application name (required)"
        &= explicit
    }
