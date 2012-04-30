{-# LANGUAGE DeriveDataTypeable, OverloadedStrings #-}

module Main (
      main
    ) where

import System.Console.CmdArgs
import Control.Concurrent      (forkIO)
import Control.Concurrent.Chan (Chan, newChan, readChan, writeChan)
import Data.Maybe      (fromJust)

import qualified Typhon.AMQP as A

version :: String
version = "0.1"

data Options = Options
    { optName :: String
    , optType :: String
    , optUri  :: String
    } deriving (Show, Data, Typeable)

options = Options
     { optName = def &= explicit &= name "name" &= help "Application name" &= typ "NAME"
     , optType = def &= explicit &= name "type" &= help "Application type" &= typ "TYPE"
     , optUri  = def &= explicit &= name "uri"  &= help "AMQP Uri"         &= typ "URI"
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
valid Options { optUri  = [] } = error "uri cannot be blank"
valid opts                     = opts

fork :: Options -> IO (Chan String)
fork opts = do
    chan <- newChan
    amqp <- A.open "typhon.ex" "typhon.q"
    forkIO $ send chan opts amqp
    return chan

recv :: Chan String -> Options -> IO ()
recv chan opts = do
    line <- getLine
    putStrLn $ "Received: " ++ line
    writeChan chan line
    recv chan opts

send :: Chan String -> Options -> A.Channel -> IO ()
send chan opts amqp = do
    line <- readChan chan
    putStrLn ("Sending: " ++ line)
    A.publish amqp "typhon.ex" "typhon.q" line
    send chan opts amqp
