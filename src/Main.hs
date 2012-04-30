{-# LANGUAGE DeriveDataTypeable #-}

module Main (
      main
    ) where

import System.Console.CmdArgs
import Control.Concurrent
import Control.Concurrent.Chan

version :: String
version = "0.1"

data Options = Options
    { optName :: String
    , optType :: String
    } deriving (Show, Data, Typeable)

options = Options
     { optName = def &= explicit &= name "name" &= help "Application name" &= typ "NAME"
     , optType = def &= explicit &= name "type" &= help "Application type" &= typ "TYPE"
     } &= summary ("Typhon " ++ version)

main :: IO ()
main = do
    opts <- parseArgs
    chan <- fork opts
    recv chan opts

parseArgs :: IO Options
parseArgs = do
    opts <- cmdArgs options
    return $ valid opts

valid :: Options -> Options
valid Options { optName = [] } = error "name cannot be blank"
valid Options { optType = [] } = error "type cannot be blank"
valid opts                     = opts

fork :: Options -> Chan String
fork opts = do
    chan <- newChan
    forkIO $ send chan opts
    chan

recv :: Chan String -> Options -> IO ()
recv chan opts = do
    line <- getLine
    putStrLn $ "Received: " ++ line
    writeChan chan line
    recv opts chan

send :: Chan String -> Options -> IO ()
send chan opts = do
    line <- readChan chan
    putStrLn ("Sending: " ++ line)
    send chan