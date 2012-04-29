module Main (
      main
    ) where

import System.Environment (getArgs)
import System.Exit        (ExitCode(..), exitWith)
import System.Console.GetOpt
import Data.Maybe         (fromMaybe)

main = do
    args <- getArgs
    let (actions, nonOpts, msgs) = getOpt RequireOrder options args
    opts <- foldl (>>=) (return defaultOptions) actions
    let Options { optInput  = input,
                  optOutput = output } = opts
    input >>= output

data Options = Options
    { optInput  :: IO String
    , optOutput :: String -> IO ()
    }

defaultOptions :: Options
defaultOptions = Options
    { optInput  = getContents
    , optOutput = putStr
    }

options :: [OptDescr (Options -> IO Options)]
options =
    [ Option ['v'] ["version"] (NoArg showVersion)         "show version number"
    , Option ['i'] ["input"]   (ReqArg readInput "FILE")   "input file to read"
    , Option ['o'] ["output"]  (ReqArg writeOutput "FILE") "output file to write"
    ]

showVersion :: a -> IO Options
showVersion _ = do
    putStrLn "Commandline example 0.1"
    exitWith ExitSuccess

readInput :: String -> Options -> IO Options
readInput arg opt = return opt { optInput = readFile arg }

writeOutput :: String -> Options -> IO Options
writeOutput arg opt = return opt { optOutput = writeFile arg }

