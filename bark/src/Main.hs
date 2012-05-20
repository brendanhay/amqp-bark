{-# LANGUAGE OverloadedStrings, RecordWildCards #-}

module Main
    ( main
    ) where

import Control.Concurrent (forkIO)
import Control.Monad.STM  (atomically)
import Data.Conduit
import Data.Conduit.TMChan
import System.IO          (stdin)
import Bark.AMQP
import Bark.Conduit
import Bark.Options
import Bark.Types

import qualified Data.ByteString.Char8  as B
import qualified Bark.Event.Exact       as E
import qualified Bark.Event.Incremental as I

main :: IO ()
main = do
    opts@Options{..} <- parseOptions
    print opts
    chan <- atomically $ newTBMChan optBound
    _    <- forkIO $ sinkStdin opts chan
    sinkEvents opts chan

--
-- Internal
--

sinkStdin :: Options -> TBMChan B.ByteString -> IO ()
sinkStdin Options{..} chan =
    runResourceT
        $  sourceHandle stdin optBuffer
        $$ sinkTBMChan chan

sinkEvents :: Options -> TBMChan B.ByteString -> IO ()
sinkEvents Options{..} chan =
    runResourceT
        $  sourceTBMChan chan
        $= tee (parser optDelimiter optStrip)
        $$ sinkAMQP (parseURI optUri) host serv
  where
    tee | optTee    = (=$= conduitShow)
        | otherwise = id
    parser = case optParser of
        Exact       -> E.conduitEvent
        Incremental -> I.conduitEvent
    host = Host $ B.pack optHost
    serv = Service $ B.pack optService