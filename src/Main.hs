{-# LANGUAGE DeriveDataTypeable, OverloadedStrings #-}

module Main (
      main
    ) where

import System.Console.CmdArgs
import System.IO          (stdin)
import Control.Concurrent (forkIO)
import Data.Maybe         (fromJust)
import Data.List          (intercalate)
import Typhon.Reader

version :: String
version = "0.1"

data Options = Options
    { optName      :: String
    , optType      :: String
    , optUri       :: String
    , optDelimiter :: String
    } deriving (Show, Data, Typeable)

options = Options
     { optName = def &= explicit &= name "name" &= help "Application name" &= typ "NAME"
     , optType = def &= explicit &= name "type" &= help "Application type" &= typ "TYPE"
     , optUri = def &= explicit &= name "uri" &= help "AMQP Uri" &= typ "URI"
     , optDelimiter = def &= explicit &= name "delimiter" &= help "Message delimiter" &= typ "DELIMITER"
     } &= summary ("Typhon " ++ version)

main = do
    buf  <- defaultBuffer
    wrtr <- defaultWriter
    forkIO (wrtr `drain` buf)
    stdin `fill` buf

parseArgs :: IO Options
parseArgs = do
    opts <- cmdArgs options
    return $ valid opts

valid :: Options -> Options
valid Options { optName = [] } = error "name cannot be blank"
valid Options { optType = [] } = error "type cannot be blank"
valid Options { optUri = [] } = error "uri cannot be blank"
valid opts = opts

-- queue :: Options -> String
-- queue opts = intercalate "." [optName opts, optType opts]

-- exchange :: Options -> String
-- exchange = optType
