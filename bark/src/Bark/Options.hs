{-# LANGUAGE DeriveDataTypeable, RecordWildCards #-}

module Bark.Options
    ( Style(..)
    , Options(..)
    , parseOptions
    ) where

import Control.Applicative ((<$>))
import Control.Monad       (when)
import Data.Version
import Network.BSD         (getHostName)
import System.Console.CmdArgs
import System.Environment  (getArgs, withArgs, getProgName)
import System.Exit         (ExitCode(..), exitWith)

data Style = Exact | Incremental deriving (Data, Typeable, Show, Eq)

data Options = Options
    { optDelimiter :: String
    , optService   :: String
    , optLocal     :: String
    , optUri       :: String
    , optBuffer    :: Int
    , optBound     :: Int
    , optParser    :: Style
    , optTee       :: Bool
    , optStrip     :: Bool
    } deriving (Data, Typeable, Show, Eq)

parseOptions :: IO Options
parseOptions = do
    args' <- getArgs
    opts  <- (if null args' then withArgs ["--help"] else id) options
    validate opts

--
-- Internal
--

options :: IO Options
options = do
    app <- getProgName
    let ver = summary $ concat [app, ": ", showVersion version]
    cmdArgs $ defaults
        &= versionArg [explicit, name "version", name "v", ver]
        &= summary ""
        &= helpArg [explicit, name "help", name "h"]
        &= program ("Usage: " ++ app)

version :: Version
version = Version
    { versionBranch = [0, 1, 0]
    , versionTags   = ["experimental"]
    }

validate :: Options -> IO Options
validate opts@Options{..} = do
    exitWhen (null optDelimiter) "--delimiter cannot be blank"
    exitWhen (null optService)   "--service cannot be blank"
    exitWhen (optBuffer <= 0)    "--buffer must be greater than zero"
    exitWhen (optBound <= 0)     "--bound must be greater than zero"

    host <- hostName
    return $ if null optLocal
              then opts{ optLocal = host }
              else opts

exitWhen :: Bool -> String -> IO ()
exitWhen p msg = when p $ putStrLn msg >> exitWith (ExitFailure 1)

hostName :: IO String
hostName = map f <$> getHostName
  where
    f '.' = '_'
    f c   = c

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

    , optLocal = ""
        &= name "hostname"
        &= typ  "HOSTNAME"
        &= help "The local hostname (default: `hostname`)"
        &= explicit

    , optUri = "amqp://guest:guest@127.0.0.1/"
        &= name "uri"
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