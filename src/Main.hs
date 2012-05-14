{-# LANGUAGE OverloadedStrings, RecordWildCards #-}

module Main (
      main
    ) where

import Control.Concurrent     (forkIO)
import Control.Monad.STM      (atomically)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Conduit
import Data.Conduit.TMChan
import Data.Maybe             (fromJust)
import System.IO              (stdin, stdout)
import Bark.AMQP
import Bark.Binary
import Bark.Options
import Bark.Parser

import qualified Data.ByteString as BS

sinkStdin :: Int -> TBMChan BS.ByteString -> IO ()
sinkStdin buffer chan =
    runResourceT
        $  sourceHandle stdin buffer
        $$ sinkTBMChan chan

parseMessages :: MonadResource m
              => Options
              -> Conduit BS.ByteString m Message
parseMessages Options{..}  =
    splitDelimiter =$= conduitMessage
  where
    splitDelimiter  = tee $ conduitSplit (fromString optDelimiter) optStrip
    tee | optTee    = (=$= conduitHandle stdout)
        | otherwise = id

sinkMessages :: TBMChan BS.ByteString -> Options -> IO ()
sinkMessages chan opts@Options{..} =
    runResourceT
        $  sourceTBMChan chan
        $= parseMessages opts
        $$ sinkAMQP uri
  where
    uri = fromJust $ parseURI "amqp://guest:guest@127.0.0.1/"

main :: IO ()
main = do
    opts@Options{..} <- parseOptions
    putStrLn $ show opts

    chan <- atomically $ newTBMChan optBound
    _    <- forkIO $ sinkStdin optBuffer chan
    sinkMessages chan opts

--
-- Development
--

sinkPrint :: MonadIO m => Sink Message m ()
sinkPrint =
    NeedInput push close
  where
    push msg = PipeM
        (liftIO (print msg) >> return (NeedInput push close))
        (return ())
    close = return ()
