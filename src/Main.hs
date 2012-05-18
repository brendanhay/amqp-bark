{-# LANGUAGE OverloadedStrings, RecordWildCards #-}

module Main
    ( main
    ) where

import Control.Concurrent (forkIO)
import Control.Monad.STM  (atomically)
import Data.Conduit
import Data.Conduit.TMChan
import Data.Maybe         (fromJust)
import Network.URI        (parseURI)
import System.IO          (stdin)
import Bark.AMQP
import Bark.Conduit
import Bark.Message.Types
import Bark.Options

import qualified Data.ByteString          as B
import qualified Bark.Message.Exact       as E
import qualified Bark.Message.Incremental as I

main :: IO ()
main = do
    opts@Options{..} <- parseOptions
    print opts
    chan <- atomically $ newTBMChan optBound
    _    <- forkIO $ sinkStdin opts chan
    sinkMessages opts chan

--
-- Internal
--

sinkStdin :: Options -> TBMChan B.ByteString -> IO ()
sinkStdin Options{..} chan =
    runResourceT
        $  sourceHandle stdin optBuffer
        $$ sinkTBMChan chan

sinkMessages :: Options -> TBMChan B.ByteString -> IO ()
sinkMessages Options{..} chan =
    runResourceT
        $  sourceTBMChan chan
        $= tee (parser optDelimiter optStrip)
        $$ sinkAMQP uri optLocal optService
  where
    tee | optTee    = (=$= conduitShow)
        | otherwise = id
    parser = case optParser of
        Exact       -> E.conduitMessage
        Incremental -> I.conduitMessage
    uri = fromJust $ parseURI optUri
