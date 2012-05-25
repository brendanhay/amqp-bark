{-# LANGUAGE OverloadedStrings, DeriveDataTypeable, RecordWildCards #-}

module Bark.Options (
      Options(..)
    , parseOptions
    ) where

import Control.Applicative    ((<$>))
import Control.Monad          (when)
import Data.Version
import Network.BSD            (getHostName)
import System.Console.CmdArgs
import System.Environment     (getArgs, withArgs, getProgName)
import System.Exit            (ExitCode(..), exitWith)
import Bark.Types

import qualified Data.ByteString.Char8 as B

data Raw = Raw
    { rawDelim         :: String
    , rawHost          :: String
    , rawService       :: String
    , rawUri           :: String
    , rawReconnect     :: Int
    , rawReadBytes     :: Int
    , rawDeliverBuffer :: Int
    , rawTee           :: Bool
    , rawStrip         :: Bool
    } deriving (Data, Typeable, Show)

data Options = Options
    { optDelim         :: String
    , optHost          :: Host
    , optService       :: Service
    , optUri           :: URI
    , optReconnect     :: Int
    , optReadBytes     :: Int
    , optDeliverBuffer :: Int
    , optTee           :: Bool
    , optStrip         :: Bool
    } deriving (Show)

parseOptions :: IO Options
parseOptions = do
    arg <- getArgs
    raw <- (if null arg then withArgs ["--help"] else id) parse
    return . conv =<< validate raw

--
-- Internal
--

parse :: IO Raw
parse = do
    app <- getProgName
    let ver = summary $ concat [app, ": ", showVersion version]
    cmdArgs $ defaults
        &= versionArg [explicit, name "version", name "v", ver]
        &= summary ""
        &= helpArg [explicit, name "help", name "h"]
        &= program ("Usage: " ++ app)

conv :: Raw -> Options
conv Raw{..} = Options
    rawDelim
    (B.pack rawHost)
    (B.pack rawService)
    (parseURI rawUri)
    (rawReconnect * 1000)
    rawReadBytes
    rawDeliverBuffer
    rawTee
    rawStrip

version :: Version
version = Version
    { versionBranch = [0, 1, 0]
    , versionTags   = ["experimental"]
    }

validate :: Raw -> IO Raw
validate raw@Raw{..} = do
    exitWhen (null rawDelim)         "--delimiter cannot be blank"
    exitWhen (null rawService)       "--service cannot be blank"
    exitWhen (rawReadBytes <= 0)     "--read-bytes must be greater than zero"
    exitWhen (rawDeliverBuffer <= 0) "--deliver-buffer must be greater than zero"
    exitWhen (rawReconnect     <= 0) ""
    addHostName raw

exitWhen :: Bool -> String -> IO ()
exitWhen p msg = when p $ putStrLn msg >> exitWith (ExitFailure 1)

addHostName :: Raw -> IO Raw
addHostName raw@Raw{..} =
    addHost <$> map f <$> getHostName
  where
    f '.' = '_'
    f c   = c
    addHost h | null rawHost = raw { rawHost = h }
              | otherwise    = raw

defaults :: Raw
defaults = Raw
    { rawDelim = "\n"
        &= name "delimiter"
        &= typ  "STRING"
        &= help "A byte or string delimiting output (default: \\n)"
        &= explicit

    , rawService = ""
        &= name "service"
        &= typ  "SERVICE"
        &= help "Application service name (required)"
        &= explicit

    , rawHost = ""
        &= name "hostname"
        &= typ  "HOSTNAME"
        &= help "Overrideable local hostname (default: `hostname`)"
        &= explicit

    , rawUri = "amqp://guest:guest@127.0.0.1/"
        &= name "uri"
        &= typ  "URI"
        &= help "AMQP URI where events will be published (default: guest@localhost)"
        &= explicit

    , rawReconnect = 500
        &= name "reconnect-delay"
        &= typ  "INT"
        &= help "Number of milliseconds to delay before attempting a reconnect (default: 500)"
        &= explicit

    , rawReadBytes = 4096
        &= name "read-bytes"
        &= typ  "BYTES"
        &= help "Size of the stdin buffer (default: 4096)"
        &= explicit

    , rawDeliverBuffer = 2048
        &= name "deliver-buffer"
        &= typ  "INT"
        &= help "Number of parsed events to buffer during publish (default: 2048)"
        &= explicit

    , rawStrip = False
        &= name "strip"
        &= help "Remove the delimiter from output (default: false)"
        &= explicit

    , rawTee = False
        &= name "tee"
        &= help "Additionally write output to stdout (default: false)"
        &= explicit
    }
