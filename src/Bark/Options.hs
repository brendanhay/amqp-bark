{-# LANGUAGE DeriveDataTypeable, RecordWildCards #-}

module Bark.Options (

    ) where

import Control.Monad      (when)
import Data.Version
import System.Console.CmdArgs
import System.Environment (getArgs, withArgs)
import System.Exit

data MyOptions = MyOptions
    { color      :: Bool
    , first_name :: String
    , age        :: Int
    , directory  :: FilePath
    } deriving (Data, Typeable, Show, Eq)

myProgOpts :: MyOptions
myProgOpts = MyOptions
    { color      = def &= help "use color"
    , first_name = def &= help "your first name"
    , age        = def &= explicit &= name "g" &= name "age" &= help "your age"
    , directory  = def &= typDir &= help "your first name"
    }

getOpts :: IO MyOptions
getOpts = cmdArgs $ myProgOpts
    &= versionArg [explicit, name "version", name "v", summary _PROGRAM_INFO]
    &= summary (_PROGRAM_INFO ++ ", " ++ _COPYRIGHT)
    &= help _PROGRAM_ABOUT
    &= helpArg [explicit, name "help", name "h"]
    &= program _PROGRAM_NAME

version :: Version
version = Version
    { versionBranch = [0, 1, 0]
    , versionTags   = []
    }

_PROGRAM_NAME = "myProg"
_PROGRAM_VERSION = "0.1.2.3"
_PROGRAM_INFO = _PROGRAM_NAME ++ " version " ++ _PROGRAM_VERSION
_PROGRAM_ABOUT = "a sample CmdArgs program for you tinkering pleasure"
_COPYRIGHT = "(C) Your Name Here 2011"

optionHandler :: MyOptions -> IO ()
optionHandler opts@MyOptions{..}  = do
    -- Take the opportunity here to weed out ugly, malformed, or invalid arguments.
    when (null first_name) $ putStrLn "--first-name is blank!" >> exitWith (ExitFailure 1)
    when (age < 5) $ putStrLn "you must be at least 5 years old to run this program" >> exitWith (ExitFailure 1)
    -- When you're done, pass the (corrected, or not) options to your actual program.
    exec opts

exec :: MyOptions -> IO ()
exec opts@MyOptions{..} = do
    putStrLn $ "Hello, " ++ firstname ++ "!"
    putStrLn $ "You are " ++ showAge ++ " years old."
    where
        firstname = if color
            then "\x1b[1;31m" ++ first_name ++ "\x1b[0m"
            else first_name
        showAge = if color
            then "\x1b[1;32m" ++ show age ++ "\x1b[0m"
            else show age