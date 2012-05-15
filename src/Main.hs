{-# LANGUAGE OverloadedStrings, RecordWildCards #-}

module Main (
      main
    ) where

import Control.Concurrent     (forkIO)
import Control.Monad.STM      (atomically)
import Data.Conduit
import Data.Conduit.TMChan
import Data.Maybe             (fromJust)
import System.IO              (stdin)
import Bark.AMQP
import Bark.Conduit
import Bark.Options

import qualified Data.ByteString          as BS
import qualified Bark.Message.Exact       as E
import qualified Bark.Message.Incremental as I


sinkStdin :: Int -> TBMChan BS.ByteString -> IO ()
sinkStdin buffer chan =
    runResourceT
        $  sourceHandle stdin buffer
        $$ sinkTBMChan chan

sinkMessages :: TBMChan BS.ByteString -> Options -> IO ()
sinkMessages chan Options{..} =
    runResourceT
        $  sourceTBMChan chan
        $= (tee $ E.conduitMessage optDelimiter optStrip)
        $$ sinkAMQP uri optName
  where
    uri = fromJust $ parseURI "amqp://guest:guest@127.0.0.1/"
    tee | optTee    = (=$= conduitShow)
        | otherwise = id

main :: IO ()
main = do
    opts@Options{..} <- parseOptions
    chan             <- atomically $ newTBMChan optBound
    _                <- forkIO $ sinkStdin optBuffer chan
    sinkMessages chan opts
