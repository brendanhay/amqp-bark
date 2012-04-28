module Typhon.Options (
    parse
  ) where

import System.Console.GetOpt

parse :: [String] -> [Flag]
parse = valid . getOpt RequireOrder options

data Flag = Version deriving (Show)

options :: [OptDescr Flag]
options =
    [ Option ['v'] ["version"] (NoArg Version) "show version number"
    ]

valid :: ([Flag], [String], [String]) -> [Flag]
valid (flags, [],  [])   = flags
valid (_,     non, [])   = error $ unrecognized non ++ header
valid (_,     _,   msgs) = error $ concat msgs ++ header

unrecognized :: [String] -> String
unrecognized = (++ "\n") . ("unrecognized arguments: " ++) . unwords

header :: String
header = usageInfo "Usage: main [OPTION...]" options

