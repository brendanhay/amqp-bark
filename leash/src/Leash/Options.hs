{-# LANGUAGE DeriveDataTypeable, RecordWildCards #-}

module Leash.Options
    ( Options(..)
    , parseOptions
    ) where

import Control.Monad       (when)
import Data.Maybe
import Data.Version
import System.Console.CmdArgs
import System.Environment  (getArgs, withArgs, getProgName)
import System.Exit         (ExitCode(..), exitWith)

data Options = Options
    { optService   :: String
    , optCategory  :: String
    , optSeverity  :: String
    , optUri       :: String
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
    exitWhen (null optService) "--service cannot be blank"
    return opts

exitWhen :: Bool -> String -> IO ()
exitWhen p msg = when p $ putStrLn msg >> exitWith (ExitFailure 1)

defaults :: Options
defaults = Options
    { optService = ""
        &= name "service"
        &= typ  "SERVICE"
        &= help "The application service name (required)"
        &= explicit

    , optCategory = ""
        &= name "category"
        &= typ  "CATEGORY"
        &= help "The category to subscribe to (default: all)"
        &= explicit

    , optSeverity = ""
        &= name "severity"
        &= typ  "SEVERITY"
        &= help "The severity to subscribe to (default: all)"
        &= explicit

    , optUri = "amqp://guest:guest@127.0.0.1/"
        &= name "uri"
        &= typ  "URI"
        &= help "The amqp uri (default: guest@localhost)"
        &= explicit
    }
