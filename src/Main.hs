module Main (
      main
    ) where

import System.Environment (getArgs)
import System.Exit        (ExitCode(..), exitWith)
import System.Console.GetOpt
import Data.Maybe         (fromMaybe)

main :: IO ()
main = do
    args <- getArgs
    let (actions, nonOpts, msgs) = getOpt RequireOrder options args
    opts <- foldl (>>=) (return defaultOptions) actions
    let Options { optName = input,
                  optType = output } = opts
    input >>= output

data Options = Options
    { optName :: IO String
    , optType :: String -> IO ()
    }

defaultOptions :: Options
defaultOptions = Options
    { optName = getContents -- "testopt"
    , optType = putStr -- "testtype"
    }

options :: [OptDescr (Options -> IO Options)]
options =
    [ Option ['u'] ["usage"]   (NoArg showUsage)       "show this message"
    , Option ['v'] ["version"] (NoArg showVersion)     "show version number"
    , Option ['n'] ["name"]    (ReqArg appName "NAME") "input file to read"
    , Option ['t'] ["type"]    (ReqArg appType "TYPE") "output file to write"
    ]

header :: String
header = "Usage: typhon [OPTION...]"

showUsage :: a -> IO Options
showUsage _ = do
    putStrLn $ usageInfo header options
    exitWith ExitSuccess

version :: String
version = "0.1"

showVersion :: a -> IO Options
showVersion _ = do
    putStrLn $ "Typhon " ++ version
    exitWith ExitSuccess

appName :: String -> Options -> IO Options
appName arg opt = return opt { optName = readFile arg }

appType :: String -> Options -> IO Options
appType arg opt = return opt { optType = writeFile arg }