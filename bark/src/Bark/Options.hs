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
    , rawReadBytes     :: Int
    , rawParseBuffer   :: Int
    , rawDeliverBuffer :: Int
    , rawReconnect     :: Int
    , rawTee           :: Bool
    , rawStrip         :: Bool
    } deriving (Data, Typeable, Show)

data Options = Options
    { optDelim         :: String
    , optHost          :: Host
    , optService       :: Service
    , optUri           :: URI
    , optReadBytes     :: Int
    , optParseBuffer   :: Int
    , optDeliverBuffer :: Int
    , optReconnect     :: Int
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
conv Raw{..} =
    Options rawDelim host serv uri rawReadBytes rawParseBuffer rawDeliverBuffer (rawReconnect * 1000) rawTee rawStrip
  where
    uri  = parseURI rawUri
    host = B.pack rawHost
    serv = B.pack rawService

version :: Version
version = Version
    { versionBranch = [0, 1, 0]
    , versionTags   = ["experimental"]
    }

validate :: Raw -> IO Raw
validate raw@Raw{..} = do
    exitWhen (null rawDelim)         "--delimiter cannot be blank"
    exitWhen (null rawService)       "--service cannot be blank"
    exitWhen (rawReadBytes     <= 0) ""
    exitWhen (rawParseBuffer   <= 0) "--parse-buffer must be greater than zero"
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
        &= help "A byte or string denoting output (default: \\n)"
        &= explicit

    , rawService = ""
        &= name "service"
        &= typ  "SERVICE"
        &= help "The application service name (required)"
        &= explicit

    , rawHost = ""
        &= name "hostname"
        &= typ  "HOSTNAME"
        &= help "The local hostname (default: `hostname`)"
        &= explicit

    , rawUri = "amqp://guest:guest@127.0.0.1/"
        &= name "uri"
        &= typ  "URI"
        &= help "The amqp uri (default: guest@localhost)"
        &= explicit

    , rawReadBytes = 4096
        &= name "read-bytes"
        &= typ  "BYTES"
        &= help "The size of the stdin buffer (default: 4096)"
        &= explicit

    , rawParseBuffer = 1024
        &= name "parse-buffer"
        &= typ  "INT"
        &= help " (default: 1024)"
        &= explicit

    , rawDeliverBuffer = 512
        &= name "deliver-buffer"
        &= typ  "INT"
        &= help " (default: 512)"
        &= explicit

    , rawReconnect = 500
        &= name "reconnect-delay"
        &= typ  "INT"
        &= help " (default: 500)"
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
