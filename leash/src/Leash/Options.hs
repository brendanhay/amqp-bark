{-# LANGUAGE OverloadedStrings, DeriveDataTypeable, RecordWildCards #-}

module Leash.Options
    ( Options(..)
    , parseOptions
    ) where

import Control.Monad          (when)
import Data.Version
import System.Console.CmdArgs
import System.Environment     (getArgs, withArgs, getProgName)
import System.Exit            (ExitCode(..), exitWith)
import Bark.Types

import qualified Data.ByteString.Char8 as B

data Raw = Raw
    { rawHost     :: String
    , rawService  :: String
    , rawCategory :: String
    , rawSeverity :: String
    , rawUri      :: String
    } deriving (Data, Typeable, Show)

data Options = Options
    { optHost     :: Host
    , optService  :: Service
    , optCategory :: Category
    , optSeverity :: Severity
    , optUri      :: URI
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
conv Raw{..} = Options host serv cat sev uri
  where
    uri  = parseURI rawUri
    host = B.pack rawHost
    serv = B.pack rawService
    cat  = B.pack rawCategory
    sev  = B.pack rawSeverity

version :: Version
version = Version
    { versionBranch = [0, 1, 0]
    , versionTags   = ["experimental"]
    }

validate :: Raw -> IO Raw
validate raw@Raw{..} = do
    exitWhen (null rawService) "--service cannot be blank"
    return raw

exitWhen :: Bool -> String -> IO ()
exitWhen p msg = when p $ putStrLn msg >> exitWith (ExitFailure 1)

defaults :: Raw
defaults = Raw
    { rawHost = ""
        &= name "host"
        &= typ  "HOST"
        &= help "The host to subscribe to (default: any)"
        &= explicit

    , rawService = ""
        &= name "service"
        &= typ  "SERVICE"
        &= help "The application service name (required)"
        &= explicit

    , rawCategory = ""
        &= name "category"
        &= typ  "CATEGORY"
        &= help "The category to subscribe to (default: all)"
        &= explicit

    , rawSeverity = ""
        &= name "severity"
        &= typ  "SEVERITY"
        &= help "The severity to subscribe to (default: all)"
        &= explicit

    , rawUri = "amqp://guest:guest@127.0.0.1/"
        &= name "uri"
        &= typ  "URI"
        &= help "The amqp uri (default: guest@localhost)"
        &= explicit
    }
