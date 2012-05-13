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

source :: TBMChan BS.ByteString -> Options -> IO ()
source chan Options{..} =
     runResourceT
         $  sourceHandle stdin optBuffer
         $$ sinkTBMChan chan

conduits :: MonadResource m
         => Options
         -> Conduit BS.ByteString m Message
conduits Options{..} =
    (tee $ split) =$= conduitMessage
  where
    split = conduitSplit (fromString optDelimiter) optStrip
    tee | optTee    = (=$= conduitHandle stdout)
        | otherwise = id

sinkPrint :: MonadIO m => Sink Message m ()
sinkPrint =
    NeedInput push close
  where
    push msg = PipeM
        (liftIO (print msg) >> return (NeedInput push close))
        (return ())
    close = return ()

sink :: TBMChan BS.ByteString -> Options -> IO ()
sink chan opts =
    runResourceT
        $  sourceTBMChan chan
        $= conduits opts
--        $$ sinkAMQP uri
        $$ sinkPrint
  where
    uri = fromJust $ parseURI "amqp://guest:guest@127.0.0.1/"

main :: IO ()
main = do
    opts@Options{..} <- parseOptions

    putStrLn $ show opts

    chan <- atomically $ newTBMChan optBound
    _    <- forkIO $ source chan opts
    sink chan opts