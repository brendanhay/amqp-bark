{-# LANGUAGE DeriveDataTypeable, RecordWildCards #-}

module Bark.Options (
      Options
    , parseOptions
    ) where

import Data.Version
import System.Console.CmdArgs
import System.Environment (getArgs, withArgs)
import System.Exit

import qualified Control.Monad as M

data Options = Options
    { optDelimiter :: String
    , optStrip     :: Bool
    , optBuffer    :: Int
    , optTee       :: Bool
    , optName       :: String
    } deriving (Data, Typeable, Show, Eq)

parseOptions :: IO Options
parseOptions = do
    args <- getArgs
    opts <- (if null args then withArgs ["--help"] else id) options
    validate opts

options :: IO Options
options = cmdArgs $ defaultOptions
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
    when (null optName)        "--name cannot be blank"
    return opts

when :: Bool -> String -> IO ()
when p msg = M.when p $ putStrLn msg >> exitWith (ExitFailure 1)

defaultOptions :: Options
defaultOptions = Options
    { optDelimiter = def
        &= name "delimiter"
        &= opt  "\n"
        &= typ  "\\n"
        &= help "The byte or string denoting a chunk of output"
        &= explicit

    , optStrip = def
        &= name "strip"
        &= help "Remove the delimiter from the output"
        &= explicit

    , optBuffer = def
        &= name "buffer"
        &= opt  "4096"
        &= typ  "4096"
        &= help "The size of the read buffer in bytes"
        &= explicit

    , optTee = def
        &= name "tee"
        &= help "Write output to stdout in addition to amqp"
        &= explicit

    , optName = def
        &= name "name"
        &= typ  "NAME"
        &= help "The application name"
        &= explicit
    }
